' ============================================================================
'  menu.bas -- menubar build, dispatch, and a few menu-triggered dialogs.
'  Uses libvt's vt_tui_menubar_* and vt_tui_form_* widgets.
' ============================================================================

' ----------------------------------------------------------------------------
'  Populate menu arrays.  Group ordering must match the MNU_* enum (1-based).
' ----------------------------------------------------------------------------
Sub menu_build(groups() As String, items() As String, counts() As Long)
    ReDim groups(4)
    ReDim counts(4)

    groups(MNU_FILE - 1) = "File"
    groups(MNU_EDIT - 1) = "Edit"
    groups(MNU_RUN  - 1) = "Run"
    groups(MNU_VIEW - 1) = "View"
    groups(MNU_HELP - 1) = "Help"

    counts(MNU_FILE - 1) = 5
    counts(MNU_EDIT - 1) = 5
    counts(MNU_RUN  - 1) = 3
    counts(MNU_VIEW - 1) = 3
    counts(MNU_HELP - 1) = 3

    Dim As Long total = counts(0) + counts(1) + counts(2) + counts(3) + counts(4)
    ReDim items(total - 1)

    Dim As Long o = 0
    ' File
    items(o + 0) = "New        Ctrl+N"
    items(o + 1) = "Open...    Ctrl+O"
    items(o + 2) = "Save       Ctrl+S"
    items(o + 3) = "Save As..."
    items(o + 4) = "Exit"
    o += counts(MNU_FILE - 1)
    ' Edit
    items(o + 0) = "Cut          Ctrl+X"
    items(o + 1) = "Copy         Ctrl+C"
    items(o + 2) = "Paste        Ctrl+V"
    items(o + 3) = "Select All   Ctrl+A"
    items(o + 4) = "Find...      Ctrl+F"
    o += counts(MNU_EDIT - 1)
    ' Run
    items(o + 0) = "Compile         F7"
    items(o + 1) = "Run             F9"
    items(o + 2) = "Compile & Run   F5"
    o += counts(MNU_RUN - 1)
    ' View
    items(o + 0) = "Editor View"
    items(o + 1) = "Chat View      F2"
    items(o + 2) = "Focus Output   F4"
    o += counts(MNU_VIEW - 1)
    ' Help
    items(o + 0) = "Settings..."
    items(o + 1) = "About"
    items(o + 2) = "Quit"
End Sub

' ----------------------------------------------------------------------------
'  About dialog.
' ----------------------------------------------------------------------------
Sub menu_show_about()
    vt_tui_dialog("About rbidefb", _
        "rbidefb " & RBIDEFB_VER & " - RB BASIC FreeBASIC IDE" & Chr(10) _
        & Chr(10) _
        & "An IDE for the RB-BASIC compiler, written in FreeBASIC" & Chr(10) _
        & "on top of the libvt text-mode UI library." & Chr(10) _
        & Chr(10) _
        & "Features:" & Chr(10) _
        & "  - 16-color DOS-style syntax highlighting" & Chr(10) _
        & "  - Compile/Run via rbbasic.exe + MinGW32" & Chr(10) _
        & "  - Built-in Claude chat assistant (F2)" & Chr(10) _
        & "  - Resizable window, F4 focus output", _
        VT_DLG_OK Or VT_TUI_WIN_SHADOW)
End Sub

' ----------------------------------------------------------------------------
'  Settings form: API key, model, max tokens, system prompt.
' ----------------------------------------------------------------------------
Sub menu_show_settings()
    Dim As Long w = 70
    Dim As Long h = 16
    Dim As Long x = (vt_cols() - w) \ 2
    Dim As Long y = (vt_rows() - h) \ 2
    If x < 1 Then x = 1
    If y < 1 Then y = 1

    Dim items(0 To 11) As vt_tui_form_item

    ' Labels (wid MUST be set, otherwise libvt skips drawing them)
    items(0).kind = VT_FORM_LABEL : items(0).x = x + 3 : items(0).y = y + 2
    items(0).val  = "Claude API key:" : items(0).align = VT_ALIGN_LEFT
    items(0).wid  = 16 : items(0).lbl_fg = VT_BLACK : items(0).lbl_bg = VT_LIGHT_GREY

    items(2).kind = VT_FORM_LABEL : items(2).x = x + 3 : items(2).y = y + 4
    items(2).val  = "Model:" : items(2).align = VT_ALIGN_LEFT
    items(2).wid  = 16 : items(2).lbl_fg = VT_BLACK : items(2).lbl_bg = VT_LIGHT_GREY

    items(4).kind = VT_FORM_LABEL : items(4).x = x + 3 : items(4).y = y + 6
    items(4).val  = "Max tokens:" : items(4).align = VT_ALIGN_LEFT
    items(4).wid  = 16 : items(4).lbl_fg = VT_BLACK : items(4).lbl_bg = VT_LIGHT_GREY

    items(6).kind = VT_FORM_LABEL : items(6).x = x + 3 : items(6).y = y + 8
    items(6).val  = "System prompt (\n for newline):" : items(6).align = VT_ALIGN_LEFT
    items(6).wid  = 40 : items(6).lbl_fg = VT_BLACK : items(6).lbl_bg = VT_LIGHT_GREY

    ' Inputs
    items(1).kind = VT_FORM_INPUT : items(1).x = x + 20 : items(1).y = y + 2
    items(1).wid  = w - 24       : items(1).val = cfgApiKey
    items(1).max_len = 256

    items(3).kind = VT_FORM_INPUT : items(3).x = x + 20 : items(3).y = y + 4
    items(3).wid  = 30           : items(3).val = cfgModel
    items(3).max_len = 64

    items(5).kind = VT_FORM_INPUT : items(5).x = x + 20 : items(5).y = y + 6
    items(5).wid  = 10           : items(5).val = Str(cfgMaxTokens)
    items(5).max_len = 8

    ' We can't fit a multi-line editor here; store sys prompt as a single
    ' string with literal "\n" escapes which config.bas decodes on load/save.
    Dim As String sp_one = cfgSysPrompt
    Dim As Long si
    For si = 1 To Len(sp_one)
        If sp_one[si - 1] = 10 Then sp_one = Left(sp_one, si - 1) & "\n" & Mid(sp_one, si + 1)
    Next
    items(7).kind = VT_FORM_INPUT : items(7).x = x + 3 : items(7).y = y + 9
    items(7).wid  = w - 6        : items(7).val = sp_one
    items(7).max_len = 2048

    ' Buttons
    items(8).kind = VT_FORM_BUTTON : items(8).x = x + w - 30
    items(8).y    = y + h - 3      : items(8).val = "  Save  "
    items(8).ret  = VT_RET_OK

    items(9).kind = VT_FORM_BUTTON : items(9).x = x + w - 18
    items(9).y    = y + h - 3      : items(9).val = " Cancel "
    items(9).ret  = VT_RET_CANCEL

    items(10).kind = VT_FORM_LABEL : items(10).x = x + 3 : items(10).y = y + h - 3
    items(10).val  = "Get a key at console.anthropic.com" : items(10).align = VT_ALIGN_LEFT
    items(10).wid  = 40 : items(10).lbl_fg = VT_DARK_GREY : items(10).lbl_bg = VT_LIGHT_GREY

    items(11).kind = VT_FORM_LABEL : items(11).x = x + 3 : items(11).y = y + h - 2
    items(11).val  = " " : items(11).align = VT_ALIGN_LEFT
    items(11).wid  = 1 : items(11).lbl_fg = VT_BLACK : items(11).lbl_bg = VT_LIGHT_GREY

    vt_tui_window(x, y, w, h, " Settings ", VT_TUI_WIN_SHADOW)

    Dim focused As Long = 1
    Dim k       As ULong
    Dim r       As Long
    Do
        vt_tui_form_draw(items(), focused)
        k = vt_inkey()
        r = vt_tui_form_handle(items(), focused, k)
        If r = VT_RET_OK Then
            cfgApiKey     = items(1).val
            cfgModel      = Trim(items(3).val)
            If Len(cfgModel) = 0 Then cfgModel = "claude-haiku-4-5"
            cfgMaxTokens  = Val(items(5).val)
            If cfgMaxTokens < 64 Then cfgMaxTokens = 1024
            ' Decode \n in system prompt back into Chr(10)
            Dim As String sp = items(7).val
            Dim As String sp2
            Dim As Long i = 1
            Do While i <= Len(sp)
                If i < Len(sp) AndAlso sp[i - 1] = Asc("\") AndAlso sp[i] = Asc("n") Then
                    sp2 &= Chr(10)
                    i += 2
                Else
                    sp2 &= Chr(sp[i - 1])
                    i += 1
                End If
            Loop
            cfgSysPrompt = sp2
            config_save()
            ' Force chat to re-init with new credentials on next send
            If chatInited Then
                claude_shutdown()
                chatInited = 0
            End If
            status_set "Settings saved", 2.5
            Exit Do
        ElseIf r = VT_RET_CANCEL OrElse r = VT_FORM_CANCEL Then
            Exit Do
        End If
        vt_sleep 10
    Loop
End Sub

' ----------------------------------------------------------------------------
'  File dialogs
' ----------------------------------------------------------------------------
Sub menu_file_open()
    Dim As String start = ExePath() & "/../examples/"
    If Len(editFilename) > 0 Then
        Dim As Long i
        For i = Len(editFilename) To 1 Step -1
            Dim As Integer c = editFilename[i - 1]
            If c = Asc("\") OrElse c = Asc("/") Then
                start = Left(editFilename, i)
                Exit For
            End If
        Next
    End If
    Dim As String pick = vt_tui_file_dialog("Open", start, "*.bas")
    If Len(pick) = 0 Then Exit Sub
    ' normalize forward slashes to backslashes on Windows
    Dim As Long j
    For j = 1 To Len(pick)
        If pick[j - 1] = Asc("/") Then pick[j - 1] = Asc("\")
    Next
    editor_load_file(pick)
    cfgLastFile = pick
    status_set "Loaded " & pick, 3.0
End Sub

Sub menu_file_save_as()
    Dim As String start = CurDir() & "/"
    If Len(editFilename) > 0 Then
        Dim As Long i
        For i = Len(editFilename) To 1 Step -1
            Dim As Integer c = editFilename[i - 1]
            If c = Asc("\") OrElse c = Asc("/") Then
                start = Left(editFilename, i)
                Exit For
            End If
        Next
    End If
    Dim As String pick = vt_tui_file_dialog("Save As", start, "*.bas")
    If Len(pick) = 0 Then Exit Sub
    Dim As Long j
    For j = 1 To Len(pick)
        If pick[j - 1] = Asc("/") Then pick[j - 1] = Asc("\")
    Next
    editor_save_file(pick)
    cfgLastFile = pick
End Sub

' ----------------------------------------------------------------------------
'  Dispatch a menu activation to the appropriate handler.
' ----------------------------------------------------------------------------
Sub menu_dispatch(grp As Long, itm As Long)
    Select Case grp
        Case MNU_FILE
            Select Case itm
                Case 1   ' New
                    If editDirty Then
                        If vt_tui_dialog("Discard?", _
                            "Discard unsaved changes?", VT_DLG_YESNO) <> VT_RET_YES Then Exit Sub
                    End If
                    editor_init_empty()
                    status_set "New file", 2.0
                Case 2   : menu_file_open()
                Case 3   ' Save
                    If Len(editFilename) > 0 Then
                        editor_save_file(editFilename)
                    Else
                        menu_file_save_as()
                    End If
                Case 4   : menu_file_save_as()
                Case 5   : appRunning = 0
            End Select

        Case MNU_EDIT
            Select Case itm
                Case 1 : editor_cut()
                Case 2 : editor_copy()
                Case 3 : editor_paste()
                Case 4 : editor_select_all()
                Case 5 : editor_find_dialog()
            End Select

        Case MNU_RUN
            Select Case itm
                Case 1 : compile_current(0)
                Case 2 : run_program()
                Case 3 : compile_current(1)
            End Select

        Case MNU_VIEW
            Select Case itm
                Case 1 : appView = VIEW_EDITOR : appFocusOutput = 0
                Case 2 : appView = IIf(appView = VIEW_CHAT, VIEW_EDITOR, VIEW_CHAT)
                Case 3 : appFocusOutput = 1
            End Select

        Case MNU_HELP
            Select Case itm
                Case 1 : menu_show_settings()
                Case 2 : menu_show_about()
                Case 3 : appRunning = 0
            End Select
    End Select
End Sub
