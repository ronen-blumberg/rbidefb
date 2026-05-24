' ============================================================================
'  config.bas -- load/save settings to rbidefb.cfg next to the exe.
'
'  Format: simple KEY=VALUE lines.  Lines beginning with ';' are ignored.
'  Multi-line system prompt is stored as a single line with literal "\n"
'  escapes (decoded on load, encoded on save).
' ============================================================================

Private Function config_path() As String
    Return ExePath() & "\rbidefb.cfg"
End Function

' Decode "\n" -> Chr(10), "\\" -> "\"
Private Function unescape_value(s As String) As String
    Dim As String r
    Dim As Long i = 1
    Do While i <= Len(s)
        Dim As Integer c = s[i - 1]
        If c = Asc("\") AndAlso i < Len(s) Then
            Dim As Integer c2 = s[i]
            If c2 = Asc("n") Then
                r &= Chr(10)
                i += 2
                Continue Do
            ElseIf c2 = Asc("\") Then
                r &= "\"
                i += 2
                Continue Do
            End If
        End If
        r &= Chr(c)
        i += 1
    Loop
    Return r
End Function

Private Function escape_value(s As String) As String
    Dim As String r
    Dim As Long i
    For i = 1 To Len(s)
        Dim As Integer c = s[i - 1]
        Select Case c
            Case Asc("\") : r &= "\\"
            Case 10       : r &= "\n"
            Case 13       ' drop
            Case Else     : r &= Chr(c)
        End Select
    Next
    Return r
End Function

' ----------------------------------------------------------------------------
'  Set sensible defaults, then overlay anything found in the config file.
' ----------------------------------------------------------------------------
Sub config_load()
    cfgApiKey     = ""
    cfgModel      = "claude-haiku-4-5"
    cfgMaxTokens  = 2048
    cfgSysPrompt  = ""
    cfgLastFile   = ""
    cfgScreenCols = 120
    cfgScreenRows = 40

    Dim As String path = config_path()
    Dim As Integer fh = FreeFile
    If Open(path For Input As #fh) <> 0 Then Exit Sub

    Dim As String ln, key, vstr
    Do Until EOF(fh)
        Line Input #fh, ln
        If Len(ln) = 0 Then Continue Do
        If ln[0] = Asc(";") OrElse ln[0] = Asc("#") Then Continue Do
        Dim As Long eq = Instr(ln, "=")
        If eq = 0 Then Continue Do
        key  = Trim(Left(ln, eq - 1))
        vstr = Mid(ln, eq + 1)
        Select Case UCase(key)
            Case "APIKEY"     : cfgApiKey     = vstr
            Case "MODEL"      : cfgModel      = vstr
            Case "MAXTOKENS"  : cfgMaxTokens  = Val(vstr)
            Case "SYSPROMPT"  : cfgSysPrompt  = unescape_value(vstr)
            Case "LASTFILE"   : cfgLastFile   = vstr
            Case "SCREENCOLS" : cfgScreenCols = Val(vstr)
            Case "SCREENROWS" : cfgScreenRows = Val(vstr)
        End Select
    Loop
    Close #fh

    If cfgScreenCols < APP_MIN_COLS Then cfgScreenCols = APP_MIN_COLS
    If cfgScreenRows < APP_MIN_ROWS Then cfgScreenRows = APP_MIN_ROWS
End Sub

Sub config_save()
    Dim As String path = config_path()
    Dim As Integer fh = FreeFile
    If Open(path For Output As #fh) <> 0 Then Exit Sub

    Print #fh, "; rbidefb settings -- auto-generated, safe to edit"
    Print #fh, "APIKEY="     & cfgApiKey
    Print #fh, "MODEL="      & cfgModel
    Print #fh, "MAXTOKENS="  & cfgMaxTokens
    Print #fh, "SYSPROMPT="  & escape_value(cfgSysPrompt)
    Print #fh, "LASTFILE="   & cfgLastFile
    Print #fh, "SCREENCOLS=" & cfgScreenCols
    Print #fh, "SCREENROWS=" & cfgScreenRows
    Close #fh
End Sub
