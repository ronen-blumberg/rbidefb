' ============================================================
'  fbclaude.bas — self-contained FreeBASIC client for the Claude API
'
'  Drop this single file into any FreeBASIC project and #include it.
'  No UI, no file I/O, no libvt — only curl + crt are required.
'
'  Quick start:
'    #include once "fbclaude.bas"
'    If claude_init("sk-ant-...", "claude-haiku-4-5", "You are a chess engine.", 1024) = 0 Then
'        Print claude_last_error() : End 1
'    End If
'    Print claude_ask("What is the best opening move for white?")
'    claude_shutdown()
'
'  Public API summary:
'    claude_init    (apiKey, model, systemPrompt, maxTokens) As Long
'    claude_ask     (userMessage)                            As String
'    claude_ask_stream(userMessage, deltaCallback)           As String
'    claude_push    (role, content)      inject history without API call
'    claude_clear   ()                   reset history, keep config
'    claude_trim    (keepLast)           drop oldest messages
'    claude_set_model(model)             swap model after init
'    claude_set_system_prompt(sysPrompt)
'    claude_set_max_tokens(maxTokens)
'    claude_set_api_key(apiKey)          rebuilds header list
'    claude_last_error()                 As String
'    claude_get_tokens(ByRef in, ByRef out)
'    claude_get_cache_read_tokens()      As Long
'    claude_get_thinking_snip()          As String (last 40 chars of thinking)
'    claude_shutdown()
'
'  JSON utilities (usable by any code that includes this module):
'    claude_json_escape   (src)                       As String
'    claude_json_unescape (src)                       As String
'    claude_json_get      (src, key, ByRef iPos)      As String
' ============================================================

#include once "crt.bi"
#include once "curl.bi"

' ============================================================
'  Internal types
' ============================================================
Type _FbclaudeMsg
    role As String
    txt  As String
End Type

Type _FbclaudeStreamSt
    lineBuf   As String
    textAccum As String
    rawAccum  As String
    thinkSnip As String
    pCb       As Sub(ByRef chunk As String)
End Type

' ============================================================
'  Shared state (all prefixed _fbclaude_ to avoid name collisions)
' ============================================================
Const _FBCLAUDE_MAX_HIST = 512

Dim Shared _fbclaude_curl      As Any Ptr
Dim Shared _fbclaude_headers   As Any Ptr
Dim Shared _fbclaude_model     As String
Dim Shared _fbclaude_sysPrompt As String
Dim Shared _fbclaude_maxTok    As Long
Dim Shared _fbclaude_history(_FBCLAUDE_MAX_HIST - 1) As _FbclaudeMsg
Dim Shared _fbclaude_histCount As Long
Dim Shared _fbclaude_lastErr    As String
Dim Shared _fbclaude_inTok     As Long
Dim Shared _fbclaude_outTok    As Long
Dim Shared _fbclaude_cacheRdTok As Long
Dim Shared _fbclaude_st        As _FbclaudeStreamSt   ' reused each call

' ============================================================
'  JSON helpers — internal implementations; exposed publicly via
'  claude_json_escape / claude_json_unescape / claude_json_get below.
' ============================================================
Function _fbclaude_esc(ByRef src As String) As String
    Dim res As String
    Dim i   As Long
    Dim c   As UByte
    For i = 0 To Len(src) - 1
        c = src[i]
        Select Case c
            Case Asc("\") : res &= "\\"
            Case Asc("""") : res &= "\"""
            Case 13       ' skip CR
            Case 10       : res &= "\n"
            Case 9        : res &= "\t"
            Case Else     : res &= Chr(c)
        End Select
    Next i
    Return res
End Function

Function _fbclaude_unesc(ByRef src As String) As String
    Dim res As String
    Dim i   As Long = 0
    While i < Len(src)
        If src[i] = Asc("\") AndAlso i + 1 < Len(src) Then
            Select Case src[i + 1]
                Case Asc("n")  : res &= Chr(10)   : i += 2 : Continue While
                Case Asc("t")  : res &= Chr(9)    : i += 2 : Continue While
                Case Asc("""") : res &= """"      : i += 2 : Continue While
                Case Asc("\")  : res &= "\"       : i += 2 : Continue While
            End Select
        End If
        res &= Chr(src[i])
        i += 1
    Wend
    Return res
End Function

' Extract the value of a JSON string field; updates iPos to past the closing quote.
Function _fbclaude_jget(ByRef src As String, ByRef key As String, ByRef iPos As Long) As String
    Dim sk  As String = """" & key & """:"
    Dim sp  As Long   = InStr(src, sk)
    If sp = 0 Then iPos = 0 : Return ""
    sp += Len(sk)
    While sp <= Len(src) AndAlso (src[sp-1] = 32 OrElse src[sp-1] = 9) : sp += 1 : Wend
    If sp > Len(src) OrElse src[sp-1] <> Asc("""") Then iPos = 0 : Return ""
    Dim res As String
    Dim i   As Long = sp
    While i < Len(src)
        If src[i] = Asc("\") Then
            res &= Chr(src[i]) : i += 1
            If i < Len(src) Then res &= Chr(src[i])
        ElseIf src[i] = Asc("""") Then
            iPos = i + 1 : Return res
        Else
            res &= Chr(src[i])
        End If
        i += 1
    Wend
    iPos = Len(src) : Return res
End Function

' Find a numeric value between a prefix and a separator (e.g. "input_tokens": N ,)
Function _fbclaude_jnum(ByRef src As String, iStart As Long, ByRef before As String, ByRef sep As String) As Long
    Dim iL As Long = InStr(iStart, src, before)
    If iL = 0 Then Return 0
    iL += Len(before)
    Dim iR As Long = InStr(iL, src, sep)
    If iR = 0 Then iR = Len(src) + 1
    Return ValInt(Mid(src, iL, iR - iL))
End Function

' ============================================================
'  JSON payload builder
' ============================================================
Function _fbclaude_payload() As String
    Dim s As String
    s  = "{"
    s &= """model"":" & Chr(34) & _fbclaude_model & Chr(34) & ","
    s &= """max_tokens"":" & Str(_fbclaude_maxTok) & ","
    s &= """stream"":true,"
    If Len(_fbclaude_sysPrompt) > 0 Then
        s &= """system"":[{""type"":""text"",""text"":"
        s &= Chr(34) & _fbclaude_esc(_fbclaude_sysPrompt) & Chr(34)
        s &= ",""cache_control"":{""type"":""ephemeral""}}],"
    End If
    s &= """messages"":["
    Dim i As Long
    For i = 0 To _fbclaude_histCount - 1
        s &= "{""role"":" & Chr(34) & _fbclaude_history(i).role & Chr(34) & ","
        s &= """content"":" & Chr(34) & _fbclaude_esc(_fbclaude_history(i).txt) & Chr(34) & "}"
        If i < _fbclaude_histCount - 1 Then s &= ","
    Next i
    s &= "]}"
    Return s
End Function

' ============================================================
'  SSE streaming write callback (called by curl)
' ============================================================
Function _fbclaude_writeCb Cdecl(buf    As UByte Ptr, _
                             sz     As Long,      _
                             nitems As Long,      _
                             ud     As Any Ptr) As Long
    Dim total As Long   = sz * nitems
    Dim chunk As String = String(total, 0)
    memcpy(StrPtr(chunk), buf, total)

    Dim st As _FbclaudeStreamSt Ptr = CPtr(_FbclaudeStreamSt Ptr, ud)
    st->lineBuf  &= chunk
    st->rawAccum &= chunk

    Dim nlPos  As Long
    Dim sLine  As String
    Dim sData  As String
    Dim sDelta As String
    Dim iPos   As Long
    Do
        nlPos = InStr(st->lineBuf, Chr(10))
        If nlPos = 0 Then Exit Do
        sLine        = Left(st->lineBuf, nlPos - 1)
        st->lineBuf  = Mid(st->lineBuf, nlPos + 1)
        If Len(sLine) > 0 AndAlso sLine[Len(sLine)-1] = 13 Then
            sLine = Left(sLine, Len(sLine) - 1)
        End If
        If Left(sLine, 6) <> "data: " Then Continue Do
        sData = Mid(sLine, 7)
        If sData = "[DONE]" Then Continue Do
        If _fbclaude_jget(sData, "type", iPos) <> "content_block_delta" Then Continue Do
        Dim dAt As Long = InStr(sData, """delta"":")
        If dAt = 0 Then Continue Do
        sDelta = Mid(sData, dAt + 8)
        Dim dType As String = _fbclaude_jget(sDelta, "type", iPos)
        If dType = "text_delta" Then
            Dim txt As String = _fbclaude_jget(sDelta, "text", iPos)
            If Len(txt) > 0 Then
                Dim u As String = _fbclaude_unesc(txt)
                st->textAccum &= u
                If st->pCb <> 0 Then st->pCb(u)
            End If
        ElseIf dType = "thinking_delta" Then
            Dim thk As String = _fbclaude_jget(sDelta, "thinking", iPos)
            If Len(thk) > 0 Then
                Dim tu As String = _fbclaude_unesc(thk)
                st->thinkSnip &= tu
                If Len(st->thinkSnip) > 40 Then
                    st->thinkSnip = Mid(st->thinkSnip, Len(st->thinkSnip) - 39)
                End If
            End If
        End If
    Loop
    Return total
End Function

' ============================================================
'  Core request (shared by claude_ask and claude_ask_stream)
' ============================================================
Function _fbclaude_doAsk(ByRef userMsg As String, _
                    pCb As Sub(ByRef chunk As String)) As String
    If _fbclaude_curl = 0 Then
        _fbclaude_lastErr = "Not initialised; call claude_init() first."
        Return ""
    End If
    If _fbclaude_histCount >= _FBCLAUDE_MAX_HIST Then
        _fbclaude_lastErr = "History full; call claude_trim() or claude_clear()."
        Return ""
    End If

    _fbclaude_history(_fbclaude_histCount).role = "user"
    _fbclaude_history(_fbclaude_histCount).txt  = userMsg
    _fbclaude_histCount += 1

    Dim sPayload As String = _fbclaude_payload()

    _fbclaude_st.lineBuf   = ""
    _fbclaude_st.textAccum = ""
    _fbclaude_st.rawAccum  = ""
    _fbclaude_st.thinkSnip = ""
    _fbclaude_st.pCb       = pCb

    curl_easy_setopt(_fbclaude_curl, CURLOPT_HTTPHEADER,    _fbclaude_headers)
    curl_easy_setopt(_fbclaude_curl, CURLOPT_URL,           "https://api.anthropic.com/v1/messages")
    curl_easy_setopt(_fbclaude_curl, CURLOPT_POST,          1)
    curl_easy_setopt(_fbclaude_curl, CURLOPT_POSTFIELDS,    StrPtr(sPayload))
    curl_easy_setopt(_fbclaude_curl, CURLOPT_POSTFIELDSIZE, Len(sPayload))
    curl_easy_setopt(_fbclaude_curl, CURLOPT_WRITEFUNCTION, @_fbclaude_writeCb)
    curl_easy_setopt(_fbclaude_curl, CURLOPT_WRITEDATA,     @_fbclaude_st)

    Dim iRes As Long = curl_easy_perform(_fbclaude_curl)

    curl_easy_setopt(_fbclaude_curl, CURLOPT_HTTPHEADER,    0)
    curl_easy_setopt(_fbclaude_curl, CURLOPT_POST,          0)
    curl_easy_setopt(_fbclaude_curl, CURLOPT_WRITEDATA,     0)
    curl_easy_setopt(_fbclaude_curl, CURLOPT_WRITEFUNCTION, 0)

    If iRes <> CURLE_OK Then
        _fbclaude_lastErr = "Network error (CURLcode " & Str(iRes) & ")"
        _fbclaude_histCount -= 1
        Return ""
    End If

    Dim httpCode As Long
    curl_easy_getinfo(_fbclaude_curl, CURLINFO_RESPONSE_CODE, @httpCode)
    If httpCode <> 200 Then
        Dim iP As Long
        _fbclaude_lastErr = "API error HTTP " & Str(httpCode) & ": " & _
                       _fbclaude_jget(_fbclaude_st.rawAccum, "message", iP)
        _fbclaude_histCount -= 1
        Return ""
    End If

    If Len(_fbclaude_st.textAccum) = 0 Then
        _fbclaude_lastErr = "Empty response from API."
        _fbclaude_histCount -= 1
        Return ""
    End If

    ' Store assistant reply in history
    _fbclaude_history(_fbclaude_histCount).role = "assistant"
    _fbclaude_history(_fbclaude_histCount).txt  = _fbclaude_st.textAccum
    _fbclaude_histCount += 1

    ' Token usage
    _fbclaude_inTok     = _fbclaude_jnum(_fbclaude_st.rawAccum, 1, """input_tokens"":", ",")
    _fbclaude_cacheRdTok = _fbclaude_jnum(_fbclaude_st.rawAccum, 1, """cache_read_input_tokens"":", ",")
    Dim dAt As Long = InStr(_fbclaude_st.rawAccum, """message_delta""")
    If dAt = 0 Then dAt = 1
    _fbclaude_outTok = _fbclaude_jnum(_fbclaude_st.rawAccum, dAt, """output_tokens"":", ",")

    _fbclaude_lastErr = ""
    Return _fbclaude_st.textAccum
End Function

' ============================================================
'  Public API
' ============================================================

' Initialise curl, store config. Returns 1 on success, 0 on failure.
Function claude_init(apiKey        As String, _
                     model         As String, _
                     systemPrompt  As String, _
                     maxTokens     As Long) As Long
    curl_global_init(CURL_GLOBAL_DEFAULT)
    _fbclaude_curl = curl_easy_init()
    If _fbclaude_curl = 0 Then
        _fbclaude_lastErr = "curl_easy_init() failed."
        Return 0
    End If
    curl_easy_setopt(_fbclaude_curl, CURLOPT_SSL_VERIFYPEER, 0)
    curl_easy_setopt(_fbclaude_curl, CURLOPT_FOLLOWLOCATION, 1)
    _fbclaude_headers = curl_slist_append(_fbclaude_headers, "Content-Type: application/json")
    _fbclaude_headers = curl_slist_append(_fbclaude_headers, "x-api-key: " & apiKey)
    _fbclaude_headers = curl_slist_append(_fbclaude_headers, "anthropic-version: 2023-06-01")
    _fbclaude_headers = curl_slist_append(_fbclaude_headers, "anthropic-beta: prompt-caching-2024-07-31")
    _fbclaude_model     = model
    _fbclaude_sysPrompt = systemPrompt
    _fbclaude_maxTok    = IIf(maxTokens > 0, maxTokens, 1024)
    _fbclaude_histCount = 0
    _fbclaude_inTok     = 0
    _fbclaude_outTok    = 0
    _fbclaude_lastErr   = ""
    Return 1
End Function

' Blocking: send userMessage, wait for full response, return it.
' Returns "" on error; call claude_last_error() for details.
Function claude_ask(userMessage As String) As String
    Return _fbclaude_doAsk(userMessage, 0)
End Function

' Streaming: deltaCallback is called for each text chunk as it arrives.
' Still returns the full assembled response when done.
' Callback signature:  Sub MyCallback(ByRef chunk As String)
Function claude_ask_stream(userMessage As String, _
                            deltaCallback As Sub(ByRef chunk As String)) As String
    Return _fbclaude_doAsk(userMessage, deltaCallback)
End Function

' Inject a message into history without making an API call.
' Useful for feeding game state to Claude before asking a question.
' role = "user" or "assistant"
Sub claude_push(role As String, content As String)
    If _fbclaude_histCount >= _FBCLAUDE_MAX_HIST Then Exit Sub
    _fbclaude_history(_fbclaude_histCount).role = role
    _fbclaude_history(_fbclaude_histCount).txt  = content
    _fbclaude_histCount += 1
End Sub

' Reset conversation history. Config (model, system prompt, etc.) unchanged.
Sub claude_clear()
    _fbclaude_histCount = 0
End Sub

' Keep only the last keepLast messages, dropping the oldest ones.
' Useful for long game sessions to avoid hitting the context window limit.
Sub claude_trim(keepLast As Long)
    If keepLast <= 0 OrElse keepLast >= _fbclaude_histCount Then Exit Sub
    Dim offset As Long = _fbclaude_histCount - keepLast
    Dim i      As Long
    For i = 0 To keepLast - 1
        _fbclaude_history(i) = _fbclaude_history(offset + i)
    Next i
    _fbclaude_histCount = keepLast
End Sub

' Swap the model after initialisation (e.g. Haiku for quick NPC chat,
' Opus for plot-critical story beats).
Sub claude_set_model(model As String)
    _fbclaude_model = model
End Sub

' Last error string from the most recent failed call.
Function claude_last_error() As String
    Return _fbclaude_lastErr
End Function

' Token counts from the most recent call (in = input, out = output).
Sub claude_get_tokens(ByRef inTok As Long, ByRef outTok As Long)
    inTok  = _fbclaude_inTok
    outTok = _fbclaude_outTok
End Sub

' Cache-read tokens from the most recent call (billed at ~10% of normal input rate).
Function claude_get_cache_read_tokens() As Long
    Return _fbclaude_cacheRdTok
End Function

' Last thinking excerpt seen during the most recent streaming call (max 40 chars).
Function claude_get_thinking_snip() As String
    Return _fbclaude_st.thinkSnip
End Function

' Change the system prompt without re-initialising (history preserved).
Sub claude_set_system_prompt(ByRef sysPrompt As String)
    _fbclaude_sysPrompt = sysPrompt
End Sub

' Change the max_tokens limit without re-initialising (history preserved).
Sub claude_set_max_tokens(maxTokens As Long)
    _fbclaude_maxTok = IIf(maxTokens > 0, maxTokens, 1024)
End Sub

' Change the API key without re-initialising (rebuilds the header list).
Sub claude_set_api_key(ByRef apiKey As String)
    If _fbclaude_headers <> 0 Then curl_slist_free_all(_fbclaude_headers) : _fbclaude_headers = 0
    _fbclaude_headers = curl_slist_append(_fbclaude_headers, "Content-Type: application/json")
    _fbclaude_headers = curl_slist_append(_fbclaude_headers, "x-api-key: " & apiKey)
    _fbclaude_headers = curl_slist_append(_fbclaude_headers, "anthropic-version: 2023-06-01")
    _fbclaude_headers = curl_slist_append(_fbclaude_headers, "anthropic-beta: prompt-caching-2024-07-31")
End Sub

' Free curl handles. Call once when your program exits.
Sub claude_shutdown()
    If _fbclaude_headers <> 0 Then curl_slist_free_all(_fbclaude_headers) : _fbclaude_headers = 0
    If _fbclaude_curl    <> 0 Then curl_easy_cleanup(_fbclaude_curl)      : _fbclaude_curl    = 0
    curl_global_cleanup()
End Sub

' ============================================================
'  JSON utilities — standalone helpers; no claude_init() needed.
' ============================================================

' Escape a plain string for embedding as a JSON string value
' (backslash, double-quote, CR stripped, LF -> \n).
Function claude_json_escape(ByRef src As String) As String
    Return _fbclaude_esc(src)
End Function

' Reverse of claude_json_escape: decode \n, \", \\ sequences.
Function claude_json_unescape(ByRef src As String) As String
    Return _fbclaude_unesc(src)
End Function

' Extract the raw (still-escaped) value of a JSON string field by key name.
' iPos is updated to the position past the closing quote on success, or 0 on failure.
' Returns "" when the key is absent or the value is not a quoted string.
Function claude_json_get(ByRef src As String, ByRef key As String, ByRef iPos As Long) As String
    Return _fbclaude_jget(src, key, iPos)
End Function
