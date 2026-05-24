' ============================================================================
'  output.bas -- compiler/run output panel.
'
'  Holds a growing list of (text, color) lines and renders them in the
'  rectangle (1, lytOutputY1)..(vt_cols, lytOutputY2) with a one-row title
'  bar at the top.
' ============================================================================

Private Sub out_grow_to(needed As Long)
    If needed <= outCapacity Then Exit Sub
    Dim As Long newcap = outCapacity
    If newcap < 32 Then newcap = 32
    Do While newcap < needed
        newcap *= 2
    Loop
    ReDim Preserve outLines(newcap - 1)
    ReDim Preserve outColors(newcap - 1)
    outCapacity = newcap
End Sub

' ----------------------------------------------------------------------------
'  Append a single line (no embedded newlines expected).
'  Caller may split multi-line strings beforehand.
' ----------------------------------------------------------------------------
Sub out_append(line_txt As String, fg As UByte = COL_OUT_FG)
    If Instr(line_txt, Chr(10)) > 0 Then
        ' split on LF
        Dim As Long i, start = 1
        For i = 1 To Len(line_txt)
            If line_txt[i - 1] = 10 Then
                Dim As String piece = Mid(line_txt, start, i - start)
                ' drop trailing CR
                If Len(piece) > 0 AndAlso piece[Len(piece) - 1] = 13 Then
                    piece = Left(piece, Len(piece) - 1)
                End If
                out_grow_to(outLineCount + 1)
                outLines(outLineCount)  = piece
                outColors(outLineCount) = fg
                outLineCount += 1
                start = i + 1
            End If
        Next
        If start <= Len(line_txt) Then
            out_grow_to(outLineCount + 1)
            outLines(outLineCount)  = Mid(line_txt, start)
            outColors(outLineCount) = fg
            outLineCount += 1
        End If
    Else
        out_grow_to(outLineCount + 1)
        outLines(outLineCount)  = line_txt
        outColors(outLineCount) = fg
        outLineCount += 1
    End If

    If outFollowTail Then
        Dim As Long vis = lytOutputY2 - (lytOutputY1 + 1) + 1
        If vis < 1 Then vis = 1
        If outLineCount > vis Then
            outTopLine = outLineCount - vis
        Else
            outTopLine = 0
        End If
    End If
End Sub

Sub out_clear()
    outLineCount = 0
    outTopLine   = 0
End Sub

' Title bar text -- shown in inverse on the first row of the output panel.
Private Function out_title_text() As String
    Return " Output  [F4 focus]  " & outLineCount & " line(s)" _
        & IIf(outFollowTail = 0, "  [scrolled]", "")
End Function

Sub out_draw()
    Dim As Long y_top = lytOutputY1
    Dim As Long y_bot = lytOutputY2
    Dim As Long w     = vt_cols()
    If y_bot < y_top OrElse w < 1 Then Exit Sub

    ' Title bar
    Dim As String title = out_title_text()
    If Len(title) > w Then title = Left(title, w)
    Dim As Long i
    For i = 1 To w
        Dim As UByte ch
        If i <= Len(title) Then ch = title[i - 1] Else ch = Asc(" ")
        vt_set_cell(i, y_top, ch, COL_OUT_HDR, VT_DARK_GREY)
    Next

    ' Body
    Dim As Long body_top = y_top + 1
    Dim As Long body_h   = y_bot - body_top + 1
    If body_h < 1 Then Exit Sub

    Dim As Long row
    For row = 0 To body_h - 1
        Dim As Long src = outTopLine + row
        Dim As Long screen_row = body_top + row
        Dim As String s = ""
        Dim As UByte fg = COL_OUT_FG
        If src < outLineCount Then
            s  = outLines(src)
            fg = outColors(src)
        End If
        Dim As Long col
        For col = 1 To w
            Dim As UByte ch
            If col <= Len(s) Then ch = s[col - 1] Else ch = Asc(" ")
            vt_set_cell(col, screen_row, ch, fg, COL_OUT_BG)
        Next
    Next
End Sub

' ----------------------------------------------------------------------------
'  Handle PgUp/PgDn/Home/End while output panel has focus.
' ----------------------------------------------------------------------------
Function out_handle(k As ULong) As Long
    If k = 0 Then Return 0
    Dim As Long sc  = VT_SCAN(k)
    Dim As Long vis = lytOutputY2 - (lytOutputY1 + 1) + 1
    If vis < 1 Then vis = 1

    Select Case sc
        Case VT_KEY_UP
            If outTopLine > 0 Then outTopLine -= 1
            outFollowTail = 0
        Case VT_KEY_DOWN
            If outTopLine + vis < outLineCount Then
                outTopLine += 1
            End If
            If outTopLine + vis >= outLineCount Then outFollowTail = 1
        Case VT_KEY_PGUP
            outTopLine -= vis
            If outTopLine < 0 Then outTopLine = 0
            outFollowTail = 0
        Case VT_KEY_PGDN
            outTopLine += vis
            If outTopLine > outLineCount - vis Then outTopLine = outLineCount - vis
            If outTopLine < 0 Then outTopLine = 0
            If outTopLine + vis >= outLineCount Then outFollowTail = 1
        Case VT_KEY_HOME
            outTopLine = 0
            outFollowTail = 0
        Case VT_KEY_END
            If outLineCount > vis Then
                outTopLine = outLineCount - vis
            Else
                outTopLine = 0
            End If
            outFollowTail = 1
        Case Else
            Return 0
    End Select
    Return 1
End Function
