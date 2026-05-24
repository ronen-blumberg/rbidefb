' ============================================================================
'  rbidefb.bas -- entry point + main event loop for the RB BASIC FreeBASIC IDE.
'
'  Compile with:   build.bat   (uses fbc32 from FreeBASIC-1.10.1+)
' ============================================================================

#include once "rbidefb.bi"

' --- bring in the other modules ---
#include once "syntax.bas"
#include once "editor.bas"
#include once "output.bas"
#include once "compiler.bas"
#include once "config.bas"
#include once "chat.bas"
#include once "menu.bas"

' ============================================================================
'  Status bar helpers
' ============================================================================
Sub status_set(msg As String, seconds As Double = 3.0)
    statusMsg      = msg
    statusMsgUntil = Timer + seconds
End Sub

Private Sub status_bar_draw()
    Dim As Long w = vt_cols()
    Dim As Long y = lytStatusRow
    Dim As Long col

    ' --- left: file name ---
    Dim As String left_s
    If Len(editFilename) > 0 Then
        Dim As Long si = 1
        Dim As Long lastSlash = 0
        For si = 1 To Len(editFilename)
            Dim As Integer c = editFilename[si - 1]
            If c = Asc("\") OrElse c = Asc("/") Then lastSlash = si
        Next
        If lastSlash > 0 Then
            left_s = " " & Mid(editFilename, lastSlash + 1)
        Else
            left_s = " " & editFilename
        End If
    Else
        left_s = " [untitled]"
    End If
    If editDirty Then left_s &= " *"

    ' --- right: row,col / count ---
    Dim As String right_s = " Ln " & (editCurRow + 1) & ", Col " & (editCurCol + 1) _
                          & " / " & editLineCount & " "

    ' --- center: status message ---
    Dim As String mid_s
    If Timer < statusMsgUntil Then mid_s = statusMsg

    ' Clear row
    For col = 1 To w
        vt_set_cell(col, y, Asc(" "), COL_STATUS_FG, COL_STATUS_BG)
    Next
    ' Left
    For col = 1 To Len(left_s)
        If col <= w Then vt_set_cell(col, y, left_s[col - 1], COL_STATUS_FG, COL_STATUS_BG)
    Next
    ' Right
    Dim As Long rcol = w - Len(right_s) + 1
    If rcol < 1 Then rcol = 1
    For col = 1 To Len(right_s)
        If rcol + col - 1 <= w Then
            vt_set_cell(rcol + col - 1, y, right_s[col - 1], COL_STATUS_FG, COL_STATUS_BG)
        End If
    Next
    ' Center (only if it doesn't collide with left/right)
    If Len(mid_s) > 0 Then
        Dim As Long mcol = (w - Len(mid_s)) \ 2 + 1
        If mcol > Len(left_s) + 2 AndAlso mcol + Len(mid_s) < rcol - 1 Then
            For col = 1 To Len(mid_s)
                vt_set_cell(mcol + col - 1, y, mid_s[col - 1], _
                            VT_BLACK Or VT_BLINK, COL_STATUS_BG)
            Next
        End If
    End If
End Sub

' ============================================================================
'  Layout (recomputed every frame)
' ============================================================================
Private Sub recompute_layout()
    Dim As Long cols = vt_cols()
    Dim As Long rows = vt_rows()
    lytMenuRow   = 1
    lytStatusRow = rows
    lytEdCols    = cols

    If appView = VIEW_EDITOR Then
        Dim As Long avail = rows - 2     ' menu + status
        Dim As Long out_h = (avail * OUTPUT_FRAC_NUM) \ OUTPUT_FRAC_DEN
        If out_h < OUTPUT_MIN_ROWS Then out_h = OUTPUT_MIN_ROWS
        If out_h > avail - 3       Then out_h = avail - 3
        If out_h < 2               Then out_h = 2
        lytEditorY1 = 2
        lytEditorY2 = rows - 1 - out_h
        lytOutputY1 = lytEditorY2 + 1
        lytOutputY2 = rows - 1
        lytEdRows   = lytEditorY2 - lytEditorY1 + 1
    Else   ' VIEW_CHAT
        Dim As Long avail = rows - 2
        Dim As Long input_h = 3
        Dim As Long btn_h   = 1
        Dim As Long sep_h   = 1
        Dim As Long chat_h  = avail - input_h - btn_h - sep_h
        If chat_h < 3 Then chat_h = 3
        lytChatY1      = 2
        lytChatY2      = lytChatY1 + chat_h - 1
        lytChatInputY1 = lytChatY2 + sep_h + 1
        lytChatInputY2 = lytChatInputY1 + input_h - 1
        lytOutputY1    = lytChatInputY2 + 1
        lytOutputY2    = lytOutputY1
    End If
End Sub

' ============================================================================
'  Global key handler (F-keys and Ctrl shortcuts that always apply).
'  Returns 1 if consumed.
' ============================================================================
Private Function handle_global_keys(k As ULong) As Long
    If k = 0 Then Return 0
    Dim As Long sc     = VT_SCAN(k)
    Dim As Long ch     = VT_CHAR(k)
    Dim As Long ctrl_h = VT_CTRL(k)
    Dim As Long alt_h  = VT_ALT(k)

    ' Alt is reserved for menubar -- never consume here.
    If alt_h Then Return 0

    Select Case sc
        Case VT_KEY_F1
            menu_show_about()
            Return 1
        Case VT_KEY_F2
            appView = IIf(appView = VIEW_CHAT, VIEW_EDITOR, VIEW_CHAT)
            appFocusOutput = 0
            Return 1
        Case VT_KEY_F4
            appFocusOutput = (appFocusOutput Xor 1)
            status_set(IIf(appFocusOutput, "Output focus: PgUp/PgDn scroll output", _
                                           "Editor focus"), 3.0)
            Return 1
        Case VT_KEY_F5
            compile_current(1)
            Return 1
        Case VT_KEY_F7
            compile_current(0)
            Return 1
        Case VT_KEY_F9
            run_program()
            Return 1
        Case VT_KEY_F10
            ' Conventional 'open menu' key -- we just leave it for the menubar
            ' which already handles Alt+letter; F10 is currently a no-op.
            Return 1
    End Select

    If ctrl_h Then
        Select Case ch
            Case Asc("N") - 64   ' Ctrl+N
                If editDirty Then
                    If vt_tui_dialog("Discard?", "Discard unsaved changes?", _
                                     VT_DLG_YESNO) <> VT_RET_YES Then Return 1
                End If
                editor_init_empty()
                status_set "New file", 2.0
                Return 1
            Case Asc("O") - 64   ' Ctrl+O
                menu_file_open()
                Return 1
            Case Asc("S") - 64   ' Ctrl+S
                If Len(editFilename) > 0 Then
                    editor_save_file(editFilename)
                Else
                    menu_file_save_as()
                End If
                Return 1
            Case Asc("Q") - 64   ' Ctrl+Q -- quit
                If editDirty Then
                    If vt_tui_dialog("Quit?", "Discard unsaved changes and quit?", _
                                     VT_DLG_YESNO) <> VT_RET_YES Then Return 1
                End If
                appRunning = 0
                Return 1
        End Select
    End If

    Return 0
End Function

' ============================================================================
'  Close callback -- prompt if the buffer is dirty.
' ============================================================================
Function on_close_cb() As Byte
    If editDirty = 0 Then Return 0   ' allow close
    If vt_tui_dialog("Quit?", _
        "Discard unsaved changes and quit?", VT_DLG_YESNO) = VT_RET_YES Then
        appRunning = 0
        Return 0
    End If
    Return 1   ' veto close
End Function

' ============================================================================
'  Splash text shown in the output panel on first run.
' ============================================================================
Private Sub show_splash()
    out_append("=== rbidefb " & RBIDEFB_VER & " - RB BASIC FreeBASIC IDE ===", COL_OUT_HDR)
    out_append("  F1=About  F2=Editor/Chat  F4=Focus Output", COL_OUT_FG)
    out_append("  F5=Compile&Run  F7=Compile  F9=Run", COL_OUT_FG)
    out_append("  Ctrl+N New  Ctrl+O Open  Ctrl+S Save  Ctrl+F Find  Ctrl+Q Quit", COL_OUT_FG)
    out_append("  Alt+F/E/R/V/H opens menu groups", COL_OUT_FG)
    out_append("", COL_OUT_FG)
    out_append("Compiler: " & compilerPath, COL_OUT_FG)
    out_append("", COL_OUT_FG)
End Sub

' ============================================================================
'  main
' ============================================================================
config_load()
compile_locate()

' --- libvt init ---
vt_screen(VT_SCREEN_0, VT_WINDOWED Or VT_RENDERER_HW)
vt_title "rbidefb - RB BASIC IDE (FreeBASIC)"
vt_screen_minimum(APP_MIN_COLS, APP_MIN_ROWS)
vt_screen_maximum(0, 0)         ' no max
vt_mouse(1)
vt_copypaste(VT_ENABLED)
vt_on_close(@on_close_cb)
vt_key_repeat(400, 30)

' --- app init ---
editor_init_empty()
out_clear()
outFollowTail = 1
outCapacity   = OUT_INIT_CAPACITY
chat_init()
appView        = VIEW_EDITOR
appRunning     = 1
appFocusOutput = 0
show_splash()

' --- restore last file if any ---
If Len(cfgLastFile) > 0 Then
    Dim As Integer fh = FreeFile
    If Open(cfgLastFile For Input As #fh) = 0 Then
        Close #fh
        editor_load_file(cfgLastFile)
    End If
End If

' --- menu storage (rebuilt each frame -- cheap string assignments) ---
ReDim menu_groups(0) As String
ReDim menu_items(0)  As String
ReDim menu_counts(0) As Long

' --- main loop ---
Do While appRunning
    recompute_layout()

    ' Rebuild menu (cheap; allows hot-changing model name in chat title etc.)
    menu_build(menu_groups(), menu_items(), menu_counts())

    ' Get input
    Dim k As ULong = vt_inkey()

    ' First chance: menubar (it handles Alt+letter, clicks, etc.)
    Dim As Long mret = vt_tui_menubar_handle(lytMenuRow, _
                                             menu_groups(), menu_items(), _
                                             menu_counts(), k)
    If mret <> 0 Then
        Dim As Long grp = VT_TUI_MENU_GROUP(mret)
        Dim As Long itm = VT_TUI_MENU_ITEM(mret)
        menu_dispatch(grp, itm)
        k = 0   ' consumed
    End If

    ' Second chance: global F-keys and Ctrl shortcuts
    If k <> 0 Then
        If handle_global_keys(k) Then k = 0
    End If

    ' Third chance: route to focused widget
    If k <> 0 Then
        If appFocusOutput Then
            If out_handle(k) Then k = 0
        End If
    End If
    If k <> 0 Then
        Select Case appView
            Case VIEW_EDITOR
                editor_handle(k)
            Case VIEW_CHAT
                chat_handle(k)
        End Select
    End If

    ' ---- DRAW ----
    vt_cls(VT_BLACK)

    ' Menu bar
    vt_tui_menubar_draw(lytMenuRow, menu_groups())

    Select Case appView
        Case VIEW_EDITOR
            editor_draw()
            out_draw()
        Case VIEW_CHAT
            chat_draw()
    End Select

    status_bar_draw()

    vt_present()
    vt_sleep(16)
Loop

' --- shutdown ---
cfgScreenCols = vt_cols()
cfgScreenRows = vt_rows()
config_save()
chat_shutdown()
vt_shutdown()
End 0
