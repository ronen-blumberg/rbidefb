' ============================================================================
'  chat.bas -- Claude chat side panel.
'
'  Layout when appView = VIEW_CHAT:
'    rows lytChatY1..lytChatY2          chat transcript (scrollable)
'    rows lytChatInputY1..lytChatInputY2 input editor (libvt tui editor)
'    row  lytOutputY1                    one-row hint
'
'  Send: Ctrl+Enter or "[ Send ]" mouse click
'  Clear: Ctrl+Shift+N or "[ Clear ]" button
'
'  Network calls are SYNCHRONOUS in v1; the UI freezes for the duration
'  of the API call (typically a second or two for haiku).
' ============================================================================

Dim Shared chatInited     As Byte
Dim Shared chatSendBtnCol As Long
Dim Shared chatClearBtnCol As Long
Dim Shared chatLastErr    As String

' ----------------------------------------------------------------------------
'  Initialise local state.  Does not touch the network.
' ----------------------------------------------------------------------------
Sub chat_init()
    chatHistoryCap = CHAT_INIT_CAPACITY
    ReDim chatHistory(chatHistoryCap - 1)
    chatHistoryCount = 0

    chatInputEd.work    = ""
    chatInputEd.cpos    = 0
    chatInputEd.top_ln  = 0
    chatInputEd.dirty   = 0
    chatInputEd.flags   = 0

    chatScrollTop   = 0
    chatStreamAccum = ""
    chatStreaming   = 0
    chatInited      = 0
End Sub

Sub chat_shutdown()
    If chatInited Then claude_shutdown()
    chatInited = 0
End Sub

' ----------------------------------------------------------------------------
'  Lazy init: actually open the Anthropic connection on first send.
'  Returns 0 on success.
' ----------------------------------------------------------------------------
Private Function chat_lazy_init() As Long
    If chatInited Then Return 0
    If Len(Trim(cfgApiKey)) = 0 Then
        chatLastErr = "No API key set. Use Settings -> Configure."
        Return 1
    End If
    Dim As String model = cfgModel
    If Len(model) = 0 Then model = "claude-haiku-4-5"
    Dim As Long mt = cfgMaxTokens
    If mt < 64 Then mt = 1024
    Dim As String sp = cfgSysPrompt
    If Len(sp) = 0 Then
        sp = "You are a helpful coding assistant inside an RB-BASIC IDE. " _
           & "RB-BASIC is a QBasic-flavored compiler that transpiles to C and " _
           & "links against MinGW32 + SDL2. Be concise, prefer code examples, " _
           & "and use the dialect shown in the user's snippets."
    End If
    If claude_init(cfgApiKey, model, sp, mt) = 0 Then
        chatLastErr = claude_last_error()
        Return 2
    End If
    chatInited = 1
    Return 0
End Function

Sub chat_grow_to(needed As Long)
    If needed <= chatHistoryCap Then Exit Sub
    Dim As Long c = chatHistoryCap
    Do While c < needed
        c *= 2
    Loop
    ReDim Preserve chatHistory(c - 1)
    chatHistoryCap = c
End Sub

Sub chat_append(role As String, txt As String)
    chat_grow_to(chatHistoryCount + 1)
    chatHistory(chatHistoryCount).role = role
    chatHistory(chatHistoryCount).txt  = txt
    chatHistoryCount += 1
End Sub

Sub chat_clear()
    chatHistoryCount = 0
    chatScrollTop    = 0
    If chatInited Then claude_clear()
    status_set "Chat cleared", 2.0
End Sub

' ----------------------------------------------------------------------------
'  Word-wrap one piece of text into a String() at the given width.
'  Each output line is at most wid chars; the array is sized exactly.
' ----------------------------------------------------------------------------
Private Sub wrap_to_lines(txt As String, wid As Long, _
                          out_lines() As String, _
                          ByRef out_count As Long)
    out_count = 0
    If wid < 1 Then wid = 1
    ' First split on existing newlines, then word-wrap each piece.
    Dim As Long start = 1
    Dim As Long i, j
    For i = 1 To Len(txt) + 1
        Dim As Integer c = IIf(i <= Len(txt), txt[i - 1], 10)
        If c = 10 OrElse i > Len(txt) Then
            Dim As String para = Mid(txt, start, i - start)
            ' Drop trailing CR
            If Len(para) > 0 AndAlso para[Len(para) - 1] = 13 Then
                para = Left(para, Len(para) - 1)
            End If
            If Len(para) = 0 Then
                ReDim Preserve out_lines(out_count)
                out_lines(out_count) = ""
                out_count += 1
            Else
                Dim As Long p = 1
                Do While p <= Len(para)
                    Dim As Long take = wid
                    If p + take - 1 > Len(para) Then take = Len(para) - p + 1
                    ' try to break on the last space within [p..p+take-1]
                    If p + take - 1 < Len(para) Then
                        For j = p + take - 1 To p Step -1
                            If para[j - 1] = Asc(" ") Then
                                take = j - p
                                Exit For
                            End If
                        Next
                        If take < 1 Then take = wid
                    End If
                    ReDim Preserve out_lines(out_count)
                    out_lines(out_count) = Mid(para, p, take)
                    out_count += 1
                    p += take
                    ' skip a single space after a soft break
                    If p <= Len(para) AndAlso para[p - 1] = Asc(" ") Then p += 1
                Loop
            End If
            start = i + 1
        End If
    Next
End Sub

' ----------------------------------------------------------------------------
'  Draw the chat panel.  Called every frame while appView = VIEW_CHAT.
' ----------------------------------------------------------------------------
Sub chat_draw()
    Dim As Long w = vt_cols()

    ' --- title bar for the chat pane ---
    Dim As String title = " Claude " & cfgModel & "  [Ctrl+Enter=Send  Ctrl+L=Clear]"
    If Len(title) > w Then title = Left(title, w)
    Dim As Long col
    For col = 1 To w
        Dim As UByte ch
        If col <= Len(title) Then ch = title[col - 1] Else ch = Asc(" ")
        vt_set_cell(col, lytChatY1, ch, COL_CHAT_SYS, VT_DARK_GREY)
    Next

    ' --- chat history area ---
    Dim As Long body_top = lytChatY1 + 1
    Dim As Long body_bot = lytChatY2
    Dim As Long body_h   = body_bot - body_top + 1
    If body_h < 1 Then body_h = 1

    ' Build all wrapped lines (text, color) up to scroll + body_h.
    ' For simplicity, build everything and slice -- typical sessions are small.
    ReDim all_lines(0) As String
    ReDim all_fg(0)    As UByte
    Dim As Long total = 0
    Dim As Long m
    For m = 0 To chatHistoryCount - 1
        Dim As String role  = chatHistory(m).role
        Dim As String body  = chatHistory(m).txt
        Dim As String hdr
        Dim As UByte fg
        Select Case role
            Case "user"      : hdr = "[ You ]"           : fg = COL_CHAT_USER
            Case "assistant" : hdr = "[ " & cfgModel & " ]" : fg = COL_CHAT_ASST
            Case Else        : hdr = "[ " & role & " ]"  : fg = COL_CHAT_SYS
        End Select

        ReDim Preserve all_lines(total)
        ReDim Preserve all_fg(total)
        all_lines(total) = hdr
        all_fg(total)    = fg
        total += 1

        ReDim wrapped(0) As String
        Dim As Long wn = 0
        wrap_to_lines(body, w - 2, wrapped(), wn)
        Dim As Long k
        For k = 0 To wn - 1
            ReDim Preserve all_lines(total)
            ReDim Preserve all_fg(total)
            all_lines(total) = "  " & wrapped(k)
            all_fg(total)    = fg
            total += 1
        Next

        ' blank line between messages
        ReDim Preserve all_lines(total)
        ReDim Preserve all_fg(total)
        all_lines(total) = ""
        all_fg(total)    = COL_CHAT_ASST
        total += 1
    Next

    ' If a stream is in flight, append the partial text as an additional line(s).
    If chatStreaming AndAlso Len(chatStreamAccum) > 0 Then
        ReDim Preserve all_lines(total)
        ReDim Preserve all_fg(total)
        all_lines(total) = "[ " & cfgModel & " ... ]"
        all_fg(total)    = COL_CHAT_ASST
        total += 1
        ReDim sw(0) As String
        Dim As Long swn = 0
        wrap_to_lines(chatStreamAccum, w - 2, sw(), swn)
        Dim As Long k2
        For k2 = 0 To swn - 1
            ReDim Preserve all_lines(total)
            ReDim Preserve all_fg(total)
            all_lines(total) = "  " & sw(k2)
            all_fg(total)    = COL_CHAT_ASST
            total += 1
        Next
    End If

    ' Stick to bottom if scroll wasn't manually moved.
    Dim As Long max_top = total - body_h
    If max_top < 0 Then max_top = 0
    If chatScrollTop > max_top Then chatScrollTop = max_top

    ' --- render visible chat lines ---
    Dim As Long row
    For row = 0 To body_h - 1
        Dim As Long src = chatScrollTop + row
        Dim As Long screen_row = body_top + row
        Dim As String s = ""
        Dim As UByte fg = COL_CHAT_ASST
        If src < total Then
            s  = all_lines(src)
            fg = all_fg(src)
        End If
        Dim As Long c
        For c = 1 To w
            Dim As UByte ch
            If c <= Len(s) Then ch = s[c - 1] Else ch = Asc(" ")
            vt_set_cell(c, screen_row, ch, fg, COL_CHAT_BG)
        Next
    Next

    ' --- input editor (a thin border above) ---
    For col = 1 To w
        vt_set_cell(col, lytChatInputY1 - 1, _
                    Asc("-"), VT_DARK_GREY, VT_BLACK)
    Next
    vt_tui_editor_draw(2, lytChatInputY1, w - 2, _
                       lytChatInputY2 - lytChatInputY1 + 1, _
                       chatInputEd)

    ' --- button row (lytOutputY1 reused as a 1-row hint strip) ---
    Dim As String btn1 = "[ Send ]"
    Dim As String btn2 = "[ Clear ]"
    chatSendBtnCol  = 2
    chatClearBtnCol = 2 + Len(btn1) + 2
    Dim As Long c2
    For c2 = 1 To w
        vt_set_cell(c2, lytOutputY1, Asc(" "), COL_STATUS_FG, COL_STATUS_BG)
    Next
    For c2 = 1 To Len(btn1)
        vt_set_cell(chatSendBtnCol + c2 - 1, lytOutputY1, _
                    btn1[c2 - 1], VT_WHITE, VT_BLUE)
    Next
    For c2 = 1 To Len(btn2)
        vt_set_cell(chatClearBtnCol + c2 - 1, lytOutputY1, _
                    btn2[c2 - 1], VT_WHITE, VT_RED)
    Next
End Sub

' ----------------------------------------------------------------------------
'  Send the current input editor contents to the API.
'  Blocking: the IDE freezes until the response comes back.
' ----------------------------------------------------------------------------
Sub chat_send_pending()
    Dim As String pending = Trim(chatInputEd.work)
    If Len(pending) = 0 Then
        status_set "Type a message first", 2.0
        Exit Sub
    End If
    If chat_lazy_init() <> 0 Then
        status_set chatLastErr, 5.0
        chat_append("system", "Error: " & chatLastErr)
        Exit Sub
    End If

    chat_append("user", pending)
    chatInputEd.work   = ""
    chatInputEd.cpos   = 0
    chatInputEd.top_ln = 0
    chatInputEd.dirty  = 0

    status_set "Sending to " & cfgModel & " ...", 30.0
    ' force one paint so the user sees "Sending..." before we block
    chat_draw()
    vt_present()

    Dim As String reply = claude_ask(pending)
    If Len(reply) = 0 Then
        Dim As String e = claude_last_error()
        chat_append("system", "API error: " & e)
        status_set "API error", 5.0
    Else
        chat_append("assistant", reply)
        Dim As Long ti, tout
        claude_get_tokens(ti, tout)
        status_set "Tokens: in=" & ti & " out=" & tout, 4.0
    End If
End Sub

' ----------------------------------------------------------------------------
'  Handle keys/mouse for the chat view.
'  Returns 1 if consumed, 0 otherwise.
' ----------------------------------------------------------------------------
Function chat_handle(k As ULong) As Long
    If k = 0 Then Return 0
    Dim As Long sc     = VT_SCAN(k)
    Dim As Long ch     = VT_CHAR(k)
    Dim As Long ctrl_h = VT_CTRL(k)
    Dim As Long alt_h  = VT_ALT(k)

    If alt_h Then Return 0    ' let menubar handle Alt

    ' Ctrl+Enter -> send (Ctrl is bit 30 of k)
    If ctrl_h AndAlso sc = VT_KEY_ENTER Then
        chat_send_pending()
        Return 1
    End If

    ' Ctrl+L -> clear
    If ctrl_h AndAlso ch = (Asc("L") - 64) Then
        chat_clear()
        Return 1
    End If

    ' Chat history scrolling
    If sc = VT_KEY_PGUP Then
        chatScrollTop -= 5
        If chatScrollTop < 0 Then chatScrollTop = 0
        Return 1
    ElseIf sc = VT_KEY_PGDN Then
        chatScrollTop += 5
        Return 1
    End If

    ' Pass everything else to the input editor widget
    Dim As Long r = vt_tui_editor_handle(2, lytChatInputY1, _
                                         vt_cols() - 2, _
                                         lytChatInputY2 - lytChatInputY1 + 1, _
                                         chatInputEd, k)
    If r = VT_FORM_CANCEL Then
        ' Escape in input editor -- swallow (don't quit the app)
        Return 1
    End If

    Return 1
End Function
