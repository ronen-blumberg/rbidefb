' ============================================================================
'  editor.bas -- custom multi-line editor with RB-BASIC syntax highlighting.
'
'  All cursor positions are 0-based internally; converted to 1-based when
'  calling vt_set_cell / vt_locate.
'
'  The editor occupies the rectangle (1, lytEditorY1)..(lytEdCols, lytEditorY2).
'  Line numbers are drawn in a margin EDITOR_MARGIN_W cells wide.
' ============================================================================

' ----------------------------------------------------------------------------
'  Growth helper
' ----------------------------------------------------------------------------
Private Sub editor_grow_to(needed As Long)
    If needed <= editCapacity Then Exit Sub
    Dim As Long newcap = editCapacity
    If newcap < 16 Then newcap = 16
    Do While newcap < needed
        newcap *= 2
    Loop
    ReDim Preserve editLines(newcap - 1)
    editCapacity = newcap
End Sub

' ----------------------------------------------------------------------------
'  Reset editor to one empty line.
' ----------------------------------------------------------------------------
Sub editor_init_empty()
    editCapacity = EDIT_INIT_CAPACITY
    ReDim editLines(editCapacity - 1)
    editLineCount    = 1
    editLines(0)     = ""
    editCurRow       = 0
    editCurCol       = 0
    editTopRow       = 0
    editLeftCol      = 0
    editSelActive    = 0
    editSelAnchorRow = 0
    editSelAnchorCol = 0
    editDirty        = 0
    editFilename     = ""
End Sub

' ----------------------------------------------------------------------------
'  Load a file.  Splits on CR/LF.  On failure, leaves the editor empty
'  and writes an error to the status bar.
' ----------------------------------------------------------------------------
Sub editor_load_file(path As String)
    Dim As Integer fh = FreeFile
    If Open(path For Input As #fh) <> 0 Then
        status_set "Cannot open: " & path, 4.0
        editor_init_empty()
        editFilename = path
        Exit Sub
    End If

    editCapacity = EDIT_INIT_CAPACITY
    ReDim editLines(editCapacity - 1)
    editLineCount = 0

    Dim As String ln
    Do Until EOF(fh)
        Line Input #fh, ln
        editor_grow_to(editLineCount + 1)
        editLines(editLineCount) = ln
        editLineCount += 1
    Loop
    Close #fh

    If editLineCount = 0 Then
        editLines(0) = ""
        editLineCount = 1
    End If

    editCurRow       = 0
    editCurCol       = 0
    editTopRow       = 0
    editLeftCol      = 0
    editSelActive    = 0
    editDirty        = 0
    editFilename     = path
End Sub

' ----------------------------------------------------------------------------
'  Save buffer to a file.  Returns 0 on success, non-zero on error.
' ----------------------------------------------------------------------------
Function editor_save_file(path As String) As Long
    Dim As Integer fh = FreeFile
    If Open(path For Output As #fh) <> 0 Then
        status_set "Cannot write: " & path, 4.0
        Return 1
    End If
    Dim As Long i
    For i = 0 To editLineCount - 1
        Print #fh, editLines(i)
    Next
    Close #fh
    editFilename = path
    editDirty    = 0
    status_set "Saved: " & path, 3.0
    Return 0
End Function

' ----------------------------------------------------------------------------
'  Clamp cursor into a valid position within the current line.
' ----------------------------------------------------------------------------
Private Sub editor_clamp_cursor()
    If editCurRow < 0 Then editCurRow = 0
    If editCurRow >= editLineCount Then editCurRow = editLineCount - 1
    Dim As Long n = Len(editLines(editCurRow))
    If editCurCol < 0 Then editCurCol = 0
    If editCurCol > n Then editCurCol = n
End Sub

' ----------------------------------------------------------------------------
'  Scroll so the cursor is visible.  Must be called after every cursor move.
' ----------------------------------------------------------------------------
Private Sub editor_ensure_visible()
    Dim As Long vis_rows = lytEditorY2 - lytEditorY1 + 1
    Dim As Long vis_cols = lytEdCols - EDITOR_MARGIN_W
    If vis_rows < 1 Then vis_rows = 1
    If vis_cols < 1 Then vis_cols = 1

    If editCurRow < editTopRow Then editTopRow = editCurRow
    If editCurRow > editTopRow + vis_rows - 1 Then editTopRow = editCurRow - vis_rows + 1
    If editTopRow < 0 Then editTopRow = 0

    If editCurCol < editLeftCol Then editLeftCol = editCurCol
    If editCurCol > editLeftCol + vis_cols - 1 Then editLeftCol = editCurCol - vis_cols + 1
    If editLeftCol < 0 Then editLeftCol = 0
End Sub

' ----------------------------------------------------------------------------
'  Selection helpers.
'  norm_* returns (r1,c1,r2,c2) where (r1,c1) <= (r2,c2) lexicographically.
' ----------------------------------------------------------------------------
Private Sub editor_sel_normalize(ByRef r1 As Long, ByRef c1 As Long, _
                                 ByRef r2 As Long, ByRef c2 As Long)
    If (editSelAnchorRow < editCurRow) _
       OrElse (editSelAnchorRow = editCurRow AndAlso editSelAnchorCol <= editCurCol) Then
        r1 = editSelAnchorRow : c1 = editSelAnchorCol
        r2 = editCurRow       : c2 = editCurCol
    Else
        r1 = editCurRow       : c1 = editCurCol
        r2 = editSelAnchorRow : c2 = editSelAnchorCol
    End If
End Sub

' Returns the selected text (concatenated with chr(10)) or "" if no selection.
Private Function editor_sel_text() As String
    If editSelActive = 0 Then Return ""
    Dim As Long r1, c1, r2, c2
    editor_sel_normalize(r1, c1, r2, c2)
    If r1 = r2 Then
        Return Mid(editLines(r1), c1 + 1, c2 - c1)
    End If
    Dim As String s = Mid(editLines(r1), c1 + 1) & Chr(10)
    Dim As Long r
    For r = r1 + 1 To r2 - 1
        s &= editLines(r) & Chr(10)
    Next
    s &= Left(editLines(r2), c2)
    Return s
End Function

' Delete the current selection.  Leaves cursor at the deletion start.
Private Sub editor_sel_delete()
    If editSelActive = 0 Then Exit Sub
    Dim As Long r1, c1, r2, c2
    editor_sel_normalize(r1, c1, r2, c2)
    If r1 = r2 Then
        editLines(r1) = Left(editLines(r1), c1) & Mid(editLines(r1), c2 + 1)
    Else
        editLines(r1) = Left(editLines(r1), c1) & Mid(editLines(r2), c2 + 1)
        ' shift remaining lines down
        Dim As Long deleted = r2 - r1
        Dim As Long i
        For i = r1 + 1 To editLineCount - 1 - deleted
            editLines(i) = editLines(i + deleted)
        Next
        editLineCount -= deleted
        If editLineCount < 1 Then
            editLineCount = 1
            editLines(0) = ""
        End If
    End If
    editCurRow    = r1
    editCurCol    = c1
    editSelActive = 0
    editDirty     = 1
End Sub

' Begin or extend selection from the given old cursor position.
Private Sub editor_sel_begin_or_extend(old_row As Long, old_col As Long)
    If editSelActive = 0 Then
        editSelAnchorRow = old_row
        editSelAnchorCol = old_col
        editSelActive    = 1
    End If
End Sub

Sub editor_select_all()
    editSelAnchorRow = 0
    editSelAnchorCol = 0
    editSelActive    = 1
    editCurRow       = editLineCount - 1
    editCurCol       = Len(editLines(editCurRow))
    editor_ensure_visible()
End Sub

' ----------------------------------------------------------------------------
'  Text insertion
' ----------------------------------------------------------------------------
Private Sub editor_insert_char(ch As Integer)
    If editSelActive Then editor_sel_delete()
    Dim As String ln = editLines(editCurRow)
    editLines(editCurRow) = Left(ln, editCurCol) & Chr(ch) & Mid(ln, editCurCol + 1)
    editCurCol += 1
    editDirty = 1
End Sub

Private Sub editor_insert_newline()
    If editSelActive Then editor_sel_delete()
    Dim As String ln = editLines(editCurRow)
    Dim As String left_part  = Left(ln, editCurCol)
    Dim As String right_part = Mid(ln, editCurCol + 1)
    editLines(editCurRow) = left_part

    ' Auto-indent: copy leading whitespace from the line we just split.
    Dim As String indent = ""
    Dim As Long i
    For i = 1 To Len(left_part)
        Dim As Integer c = left_part[i - 1]
        If c = Asc(" ") OrElse c = Asc(Chr(9)) Then
            indent &= Chr(c)
        Else
            Exit For
        End If
    Next

    editor_grow_to(editLineCount + 1)
    ' shift lines down
    For i = editLineCount To editCurRow + 2 Step -1
        editLines(i) = editLines(i - 1)
    Next
    editLines(editCurRow + 1) = indent & right_part
    editLineCount += 1
    editCurRow += 1
    editCurCol = Len(indent)
    editDirty  = 1
End Sub

' Insert arbitrary text (e.g. paste), handling embedded newlines.
Sub editor_insert_text(s As String)
    If Len(s) = 0 Then Exit Sub
    If editSelActive Then editor_sel_delete()
    Dim As Long i
    For i = 1 To Len(s)
        Dim As Integer c = s[i - 1]
        Select Case c
            Case 10
                editor_insert_newline()
            Case 13
                ' ignore CR; let LF do the work
            Case 9
                ' Expand tab to spaces (keeps display column count stable)
                Dim As Long pad = TAB_WIDTH - (editCurCol Mod TAB_WIDTH)
                Dim As Long j
                For j = 1 To pad
                    editor_insert_char(Asc(" "))
                Next
            Case Else
                If c >= 32 Then editor_insert_char(c)
        End Select
    Next
End Sub

' ----------------------------------------------------------------------------
'  Backspace
' ----------------------------------------------------------------------------
Private Sub editor_backspace()
    If editSelActive Then
        editor_sel_delete()
        Exit Sub
    End If
    If editCurCol > 0 Then
        Dim As String ln = editLines(editCurRow)
        editLines(editCurRow) = Left(ln, editCurCol - 1) & Mid(ln, editCurCol + 1)
        editCurCol -= 1
        editDirty = 1
    ElseIf editCurRow > 0 Then
        ' Join with previous line
        Dim As Long prev_len = Len(editLines(editCurRow - 1))
        editLines(editCurRow - 1) = editLines(editCurRow - 1) & editLines(editCurRow)
        Dim As Long i
        For i = editCurRow To editLineCount - 2
            editLines(i) = editLines(i + 1)
        Next
        editLineCount -= 1
        editCurRow -= 1
        editCurCol  = prev_len
        editDirty   = 1
    End If
End Sub

' ----------------------------------------------------------------------------
'  Delete (forward)
' ----------------------------------------------------------------------------
Private Sub editor_delete_fwd()
    If editSelActive Then
        editor_sel_delete()
        Exit Sub
    End If
    Dim As String ln = editLines(editCurRow)
    If editCurCol < Len(ln) Then
        editLines(editCurRow) = Left(ln, editCurCol) & Mid(ln, editCurCol + 2)
        editDirty = 1
    ElseIf editCurRow < editLineCount - 1 Then
        editLines(editCurRow) = ln & editLines(editCurRow + 1)
        Dim As Long i
        For i = editCurRow + 1 To editLineCount - 2
            editLines(i) = editLines(i + 1)
        Next
        editLineCount -= 1
        editDirty = 1
    End If
End Sub

' ----------------------------------------------------------------------------
'  Word boundary helpers (for Ctrl+Left / Ctrl+Right)
' ----------------------------------------------------------------------------
Private Function is_word_char(c As Integer) As Integer
    Return (c >= Asc("A") AndAlso c <= Asc("Z")) _
        OrElse (c >= Asc("a") AndAlso c <= Asc("z")) _
        OrElse (c >= Asc("0") AndAlso c <= Asc("9")) _
        OrElse c = Asc("_") OrElse c = Asc("$") _
        OrElse c = Asc("%") OrElse c = Asc("&") _
        OrElse c = Asc("!") OrElse c = Asc("#")
End Function

Private Sub editor_word_left()
    If editCurCol = 0 Then
        If editCurRow > 0 Then
            editCurRow -= 1
            editCurCol  = Len(editLines(editCurRow))
        End If
        Exit Sub
    End If
    Dim As String ln = editLines(editCurRow)
    ' skip non-word, then word
    Do While editCurCol > 0 AndAlso Not is_word_char(ln[editCurCol - 1])
        editCurCol -= 1
    Loop
    Do While editCurCol > 0 AndAlso is_word_char(ln[editCurCol - 1])
        editCurCol -= 1
    Loop
End Sub

Private Sub editor_word_right()
    Dim As String ln = editLines(editCurRow)
    Dim As Long n = Len(ln)
    If editCurCol >= n Then
        If editCurRow < editLineCount - 1 Then
            editCurRow += 1
            editCurCol  = 0
        End If
        Exit Sub
    End If
    Do While editCurCol < n AndAlso is_word_char(ln[editCurCol])
        editCurCol += 1
    Loop
    Do While editCurCol < n AndAlso Not is_word_char(ln[editCurCol])
        editCurCol += 1
    Loop
End Sub

' ----------------------------------------------------------------------------
'  Clipboard
' ----------------------------------------------------------------------------
Sub editor_copy()
    If editSelActive = 0 Then Exit Sub
    editClipboard = editor_sel_text()
    status_set "Copied " & Len(editClipboard) & " chars", 2.0
End Sub

Sub editor_cut()
    If editSelActive = 0 Then Exit Sub
    editClipboard = editor_sel_text()
    editor_sel_delete()
    editor_ensure_visible()
    status_set "Cut " & Len(editClipboard) & " chars", 2.0
End Sub

Sub editor_paste()
    If Len(editClipboard) = 0 Then Exit Sub
    editor_insert_text(editClipboard)
    editor_ensure_visible()
End Sub

' ----------------------------------------------------------------------------
'  Find (simple forward case-insensitive)
' ----------------------------------------------------------------------------
Private Sub editor_find_next(needle As String)
    If Len(needle) = 0 Then Exit Sub
    Dim As String un = UCase(needle)
    Dim As Long start_row = editCurRow
    Dim As Long start_col = editCurCol + 1
    Dim As Long r
    Dim As Long match_pos
    For r = start_row To editLineCount - 1
        Dim As Long off = IIf(r = start_row, start_col, 1)
        match_pos = Instr(off, UCase(editLines(r)), un)
        If match_pos > 0 Then
            editCurRow = r
            editCurCol = match_pos - 1
            editSelAnchorRow = r
            editSelAnchorCol = match_pos - 1 + Len(needle)
            editSelActive    = 1
            ' make sure the END of the match is visible
            Dim As Long save_col = editCurCol
            editCurCol = editSelAnchorCol
            editor_ensure_visible()
            editCurCol = save_col
            editor_ensure_visible()
            status_set "Found at line " & (r + 1), 2.5
            Exit Sub
        End If
    Next
    ' wrap-around from top
    For r = 0 To start_row
        match_pos = Instr(1, UCase(editLines(r)), un)
        If match_pos > 0 Then
            editCurRow = r
            editCurCol = match_pos - 1
            editSelAnchorRow = r
            editSelAnchorCol = match_pos - 1 + Len(needle)
            editSelActive    = 1
            editor_ensure_visible()
            status_set "Found at line " & (r + 1) & " (wrapped)", 2.5
            Exit Sub
        End If
    Next
    status_set "Not found: " & needle, 3.0
End Sub

Sub editor_find_dialog()
    ' Build a small form with one input + OK/Cancel
    Dim items(2) As vt_tui_form_item

    items(0).kind    = VT_FORM_INPUT
    items(0).x       = vt_cols() \ 2 - 20
    items(0).y       = vt_rows() \ 2
    items(0).wid     = 40
    items(0).val     = editFindLast
    items(0).max_len = 256

    items(1).kind = VT_FORM_BUTTON
    items(1).x    = vt_cols() \ 2 - 14
    items(1).y    = vt_rows() \ 2 + 2
    items(1).val  = "  OK  "
    items(1).ret  = VT_RET_OK

    items(2).kind = VT_FORM_BUTTON
    items(2).x    = vt_cols() \ 2 + 4
    items(2).y    = vt_rows() \ 2 + 2
    items(2).val  = "Cancel"
    items(2).ret  = VT_RET_CANCEL

    vt_tui_window(vt_cols() \ 2 - 25, vt_rows() \ 2 - 2, 50, 6, " Find ", VT_TUI_WIN_SHADOW)
    vt_color VT_BLACK, VT_LIGHT_GREY
    vt_locate vt_rows() \ 2 - 1, vt_cols() \ 2 - 22
    vt_print "Search for:"

    Dim focused As Long = 0
    Dim k       As ULong
    Dim r       As Long
    Do
        vt_tui_form_draw(items(), focused)
        k = vt_inkey()
        r = vt_tui_form_handle(items(), focused, k)
        If r = VT_RET_OK Then
            editFindLast = items(0).val
            editor_find_next(editFindLast)
            Exit Do
        ElseIf r = VT_RET_CANCEL OrElse r = VT_FORM_CANCEL Then
            Exit Do
        End If
        vt_sleep 10
    Loop
End Sub

' ============================================================================
'  Render
' ============================================================================
Sub editor_draw()
    Dim As Long vis_rows = lytEditorY2 - lytEditorY1 + 1
    Dim As Long vis_cols = lytEdCols - EDITOR_MARGIN_W
    If vis_rows < 1 OrElse vis_cols < 1 Then Exit Sub

    Dim As Long r1, c1, r2, c2
    Dim As Byte have_sel = editSelActive
    If have_sel Then editor_sel_normalize(r1, c1, r2, c2)

    Dim As Long row, col, src_row
    Dim As String ln
    ReDim As UByte colors(0)

    For row = 0 To vis_rows - 1
        src_row = editTopRow + row
        Dim As Long screen_row = lytEditorY1 + row
        Dim As Long screen_col_base = 1   ' margin start

        ' --- margin: line number ---
        Dim As String lbl
        If src_row < editLineCount Then
            lbl = Right(Space(EDITOR_MARGIN_W - 1) & Str(src_row + 1), EDITOR_MARGIN_W - 1) & " "
        Else
            lbl = Space(EDITOR_MARGIN_W)
        End If
        For col = 0 To EDITOR_MARGIN_W - 1
            vt_set_cell(col + 1, screen_row, lbl[col], COL_LINENO, COL_LINENO_BG)
        Next

        ' --- content area ---
        If src_row >= editLineCount Then
            ' empty row past EOF -- fill with background
            For col = 0 To vis_cols - 1
                vt_set_cell(EDITOR_MARGIN_W + col + 1, screen_row, _
                            Asc(" "), COL_TEXT, COL_BG)
            Next
        Else
            ln = editLines(src_row)
            Dim As Long n = Len(ln)
            If n > 0 Then
                ReDim colors(n - 1)
                Dim As Long i
                For i = 0 To n - 1
                    colors(i) = COL_TEXT
                Next
                syntax_tokenize_line(ln, colors())
            End If

            For col = 0 To vis_cols - 1
                Dim As Long src_col = editLeftCol + col
                Dim As UByte ch     = Asc(" ")
                Dim As UByte fg     = COL_TEXT
                Dim As UByte bg     = COL_BG

                If src_col < n Then
                    ch = ln[src_col]
                    fg = colors(src_col)
                End If

                ' Selection overlay?
                If have_sel Then
                    Dim As Byte in_sel = 0
                    If src_row > r1 AndAlso src_row < r2 Then
                        in_sel = 1
                    ElseIf r1 = r2 AndAlso src_row = r1 Then
                        If src_col >= c1 AndAlso src_col < c2 Then in_sel = 1
                    ElseIf src_row = r1 AndAlso src_col >= c1 Then
                        in_sel = 1
                    ElseIf src_row = r2 AndAlso src_col < c2 Then
                        in_sel = 1
                    End If
                    If in_sel Then
                        fg = COL_SEL_FG
                        bg = COL_SEL_BG
                    End If
                End If

                vt_set_cell(EDITOR_MARGIN_W + col + 1, screen_row, ch, fg, bg)
            Next
        End If
    Next

    ' --- cursor placement (so it blinks at the right cell) ---
    Dim As Long cur_screen_row = lytEditorY1 + (editCurRow - editTopRow)
    Dim As Long cur_screen_col = EDITOR_MARGIN_W + (editCurCol - editLeftCol) + 1
    If cur_screen_row >= lytEditorY1 AndAlso cur_screen_row <= lytEditorY2 _
       AndAlso cur_screen_col >= EDITOR_MARGIN_W + 1 AndAlso cur_screen_col <= lytEdCols Then
        vt_locate(cur_screen_row, cur_screen_col, 1)
    Else
        vt_locate(1, 1, 0)
    End If
End Sub

' ============================================================================
'  Key handler.
'  Returns 1 if the key was consumed, 0 otherwise.
' ============================================================================
Function editor_handle(k As ULong) As Long
    If k = 0 Then Return 0

    Dim As Long sc        = VT_SCAN(k)
    Dim As Long ch        = VT_CHAR(k)
    Dim As Long shift_h   = VT_SHIFT(k)
    Dim As Long ctrl_h    = VT_CTRL(k)
    Dim As Long alt_h     = VT_ALT(k)
    Dim As Long old_row   = editCurRow
    Dim As Long old_col   = editCurCol
    Dim As Byte consumed  = 1

    ' Alt-anything -- let the menubar take it.
    If alt_h Then Return 0

    Select Case sc
        Case VT_KEY_UP
            If shift_h Then editor_sel_begin_or_extend(old_row, old_col) Else editSelActive = 0
            If editCurRow > 0 Then editCurRow -= 1
            editor_clamp_cursor()
            editor_ensure_visible()

        Case VT_KEY_DOWN
            If shift_h Then editor_sel_begin_or_extend(old_row, old_col) Else editSelActive = 0
            If editCurRow < editLineCount - 1 Then editCurRow += 1
            editor_clamp_cursor()
            editor_ensure_visible()

        Case VT_KEY_LEFT
            If shift_h Then editor_sel_begin_or_extend(old_row, old_col) Else editSelActive = 0
            If ctrl_h Then
                editor_word_left()
            Else
                If editCurCol > 0 Then
                    editCurCol -= 1
                ElseIf editCurRow > 0 Then
                    editCurRow -= 1
                    editCurCol  = Len(editLines(editCurRow))
                End If
            End If
            editor_ensure_visible()

        Case VT_KEY_RIGHT
            If shift_h Then editor_sel_begin_or_extend(old_row, old_col) Else editSelActive = 0
            If ctrl_h Then
                editor_word_right()
            Else
                Dim As Long ln_len = Len(editLines(editCurRow))
                If editCurCol < ln_len Then
                    editCurCol += 1
                ElseIf editCurRow < editLineCount - 1 Then
                    editCurRow += 1
                    editCurCol  = 0
                End If
            End If
            editor_ensure_visible()

        Case VT_KEY_HOME
            If shift_h Then editor_sel_begin_or_extend(old_row, old_col) Else editSelActive = 0
            If ctrl_h Then
                editCurRow = 0
                editCurCol = 0
            Else
                ' First go to first non-whitespace, then to col 0 on repeat.
                Dim As String ln = editLines(editCurRow)
                Dim As Long first_ns = 0
                Do While first_ns < Len(ln) AndAlso (ln[first_ns] = Asc(" ") OrElse ln[first_ns] = Asc(Chr(9)))
                    first_ns += 1
                Loop
                If editCurCol <> first_ns Then editCurCol = first_ns Else editCurCol = 0
            End If
            editor_ensure_visible()

        Case VT_KEY_END
            If shift_h Then editor_sel_begin_or_extend(old_row, old_col) Else editSelActive = 0
            If ctrl_h Then
                editCurRow = editLineCount - 1
                editCurCol = Len(editLines(editCurRow))
            Else
                editCurCol = Len(editLines(editCurRow))
            End If
            editor_ensure_visible()

        Case VT_KEY_PGUP
            If shift_h Then editor_sel_begin_or_extend(old_row, old_col) Else editSelActive = 0
            Dim As Long vis = lytEditorY2 - lytEditorY1 + 1
            editCurRow -= vis
            If editCurRow < 0 Then editCurRow = 0
            editor_clamp_cursor()
            editor_ensure_visible()

        Case VT_KEY_PGDN
            If shift_h Then editor_sel_begin_or_extend(old_row, old_col) Else editSelActive = 0
            Dim As Long visd = lytEditorY2 - lytEditorY1 + 1
            editCurRow += visd
            If editCurRow > editLineCount - 1 Then editCurRow = editLineCount - 1
            editor_clamp_cursor()
            editor_ensure_visible()

        Case VT_KEY_BKSP
            editor_backspace()
            editor_ensure_visible()

        Case VT_KEY_DEL
            editor_delete_fwd()
            editor_ensure_visible()

        Case VT_KEY_ENTER
            editor_insert_newline()
            editor_ensure_visible()

        Case VT_KEY_TAB
            ' Insert TAB_WIDTH spaces (or pad to next stop)
            Dim As Long pad = TAB_WIDTH - (editCurCol Mod TAB_WIDTH)
            Dim As Long j
            For j = 1 To pad
                editor_insert_char(Asc(" "))
            Next
            editor_ensure_visible()

        Case Else
            ' Ctrl shortcuts (C/X/V/A/F/etc) -- only when ch is in 1..26 range
            If ctrl_h AndAlso ch >= 1 AndAlso ch <= 26 Then
                Select Case ch
                    Case Asc("C") - 64 : editor_copy()
                    Case Asc("X") - 64 : editor_cut()
                    Case Asc("V") - 64 : editor_paste()
                    Case Asc("A") - 64 : editor_select_all()
                    Case Asc("F") - 64 : editor_find_dialog()
                    Case Else
                        consumed = 0
                End Select
                editor_ensure_visible()
                Return consumed
            End If

            ' Printable character
            If ch >= 32 AndAlso ch <= 126 Then
                editor_insert_char(ch)
                editor_ensure_visible()
            Else
                consumed = 0
            End If
    End Select

    Return consumed
End Function
