' ============================================================================
'  syntax.bas -- RB BASIC tokenizer for the editor.
'  Produces a per-character color array for a single source line.
' ============================================================================

' Static keyword table (UPPERCASE).  Copied verbatim from rbide's
' BASIC_KEYWORDS list (mainwindow.cpp:79) so the two IDEs stay in sync.
Static Shared rb_keywords(...) As String * 16 = { _
    "ABS", "ACCESS", "ALIAS", "AND", "ANY", "APPEND", "AS", "ASC", "ATN", _
    "BASE", "BEEP", "BINARY", "BLOAD", "BSAVE", _
    "CALL", "CALLS", "CASE", "CDBL", "CDECL", "CHAIN", "CHDIR", "CHR$", _
    "CINT", "CIRCLE", "CLEAR", "CLNG", "CLOSE", "CLS", _
    "COLOR", "COM", "COMMAND$", "COMMON", "CONST", "COS", "CSNG", "CSRLIN", _
    "CVD", "CVDMBF", "CVI", "CVL", "CVS", "CVSMBF", _
    "DATA", "DATE$", "DECLARE", "DEF", "DEFDBL", "DEFINT", "DEFLNG", _
    "DEFSNG", "DEFSTR", "DELETE", "DIM", "DO", "DOUBLE", "DRAW", _
    "ELSE", "ELSEIF", "END", "ENVIRON", "ENVIRON$", "EOF", "EQV", "ERASE", _
    "ERDEV", "ERDEV$", "ERL", "ERR", "ERROR", "EXIT", "EXP", _
    "FIELD", "FILES", "FIX", "FOR", "FRE", "FREEFILE", "FUNCTION", _
    "GET", "GOSUB", "GOTO", _
    "HEX$", _
    "IF", "IMP", "INKEY$", "INP", "INPUT", "INPUT$", "INSTR", "INT", _
    "INTEGER", "IOCTL", "IOCTL$", "IS", _
    "KEY", "KILL", _
    "LBOUND", "LCASE$", "LEFT$", "LEN", "LET", "LINE", "LIST", "LOC", _
    "LOCAL", "LOCATE", "LOCK", "LOF", "LOG", "LONG", "LOOP", "LPOS", _
    "LPRINT", "LSET", "LTRIM$", _
    "MID$", "MKD$", "MKDMBF$", "MKI$", "MKL$", "MKS$", "MKSMBF$", "MOD", _
    "NAME", "NEXT", "NOT", _
    "OCT$", "OFF", "ON", "OPEN", "OPTION", "OR", "OUT", "OUTPUT", _
    "PAINT", "PALETTE", "PCOPY", "PEEK", "PEN", "PLAY", "PMAP", "POINT", _
    "POKE", "POS", "PRESET", "PRINT", "PSET", "PUT", _
    "RANDOM", "RANDOMIZE", "READ", "REDIM", "REM", "RESET", "RESTORE", _
    "RESUME", "RETURN", "RIGHT$", "RMDIR", "RND", "RSET", "RTRIM$", "RUN", _
    "SADD", "SCREEN", "SEEK", "SEG", "SELECT", "SETMEM", "SGN", "SHARED", _
    "SHELL", "SIN", "SINGLE", "SLEEP", "SOUND", "SPACE$", "SPC", "SQR", _
    "STATIC", "STEP", "STICK", "STOP", "STR$", "STRING", "STRING$", _
    "SUB", "SWAP", "SYSTEM", _
    "TAB", "TAN", "THEN", "TIME$", "TIMER", "TO", "TROFF", "TRON", "TYPE", _
    "UBOUND", "UCASE$", "UEVENT", "UNLOCK", "UNTIL", "USING", _
    "VAL", "VARPTR", "VARPTR$", "VARSEG", "VIEW", _
    "WAIT", "WEND", "WHILE", "WIDTH", "WINDOW", "WRITE", _
    "XOR" _
}

' ----------------------------------------------------------------------------
'  Is c an ASCII letter (A..Z, a..z)?
' ----------------------------------------------------------------------------
Private Function is_alpha(c As Integer) As Integer
    Return (c >= Asc("A") AndAlso c <= Asc("Z")) _
        OrElse (c >= Asc("a") AndAlso c <= Asc("z"))
End Function

' ----------------------------------------------------------------------------
'  Is c an ASCII digit?
' ----------------------------------------------------------------------------
Private Function is_digit(c As Integer) As Integer
    Return (c >= Asc("0") AndAlso c <= Asc("9"))
End Function

' ----------------------------------------------------------------------------
'  Is c a hex digit?
' ----------------------------------------------------------------------------
Private Function is_hex_digit(c As Integer) As Integer
    Return is_digit(c) _
        OrElse (c >= Asc("A") AndAlso c <= Asc("F")) _
        OrElse (c >= Asc("a") AndAlso c <= Asc("f"))
End Function

' ----------------------------------------------------------------------------
'  Linear-scan keyword lookup (case-insensitive).
'  word should already be UCase'd.
' ----------------------------------------------------------------------------
Private Function is_keyword(word As String) As Integer
    Dim As Integer i
    For i = 0 To UBound(rb_keywords)
        If rb_keywords(i) = word Then Return 1
    Next
    Return 0
End Function

' ----------------------------------------------------------------------------
'  Public entry point.
'  Fills colors() with one color index per character of line_txt.
'  colors() is assumed to have been ReDim'd by the caller to at
'  least Len(line_txt) entries.
' ----------------------------------------------------------------------------
Sub syntax_tokenize_line(line_txt As String, colors() As UByte)
    Dim As Long n = Len(line_txt)
    If n = 0 Then Exit Sub

    Dim As Long i, j, start
    Dim As Integer c
    Dim As String word

    i = 0
    Do While i < n
        c = line_txt[i]

        ' --- Quote: ' starts a comment to end of line ---
        If c = Asc("'") Then
            For j = i To n - 1
                colors(j) = COL_COMMENT
            Next
            Exit Do
        End If

        ' --- String literal "..." ---
        If c = Asc("""") Then
            start = i
            i += 1
            Do While i < n
                If line_txt[i] = Asc("""") Then
                    i += 1
                    Exit Do
                End If
                i += 1
            Loop
            For j = start To i - 1
                If j < n Then colors(j) = COL_STRING
            Next
            Continue Do
        End If

        ' --- Hex / octal literal &H... &O... ---
        If c = Asc("&") AndAlso (i + 1) < n Then
            Dim As Integer c2 = line_txt[i + 1]
            If c2 = Asc("H") OrElse c2 = Asc("h") _
               OrElse c2 = Asc("O") OrElse c2 = Asc("o") Then
                start = i
                i += 2
                Do While i < n AndAlso is_hex_digit(line_txt[i])
                    i += 1
                Loop
                For j = start To i - 1
                    colors(j) = COL_NUMBER
                Next
                Continue Do
            End If
        End If

        ' --- Numeric literal ---
        If is_digit(c) _
           OrElse (c = Asc(".") AndAlso (i + 1) < n AndAlso is_digit(line_txt[i + 1])) Then
            start = i
            Do While i < n AndAlso (is_digit(line_txt[i]) OrElse line_txt[i] = Asc("."))
                i += 1
            Loop
            ' exponent
            If i < n AndAlso (line_txt[i] = Asc("e") OrElse line_txt[i] = Asc("E")) Then
                i += 1
                If i < n AndAlso (line_txt[i] = Asc("+") OrElse line_txt[i] = Asc("-")) Then
                    i += 1
                End If
                Do While i < n AndAlso is_digit(line_txt[i])
                    i += 1
                Loop
            End If
            ' optional type suffix
            If i < n Then
                Dim As Integer cs = line_txt[i]
                If cs = Asc("!") OrElse cs = Asc("#") OrElse cs = Asc("&") OrElse cs = Asc("%") Then
                    i += 1
                End If
            End If
            For j = start To i - 1
                colors(j) = COL_NUMBER
            Next
            Continue Do
        End If

        ' --- Identifier / keyword ---
        If is_alpha(c) OrElse c = Asc("_") Then
            start = i
            Do While i < n AndAlso (is_alpha(line_txt[i]) OrElse is_digit(line_txt[i]) OrElse line_txt[i] = Asc("_"))
                i += 1
            Loop
            ' optional type suffix ($ % & ! #) -- part of QB string-fn names
            If i < n Then
                Dim As Integer cs = line_txt[i]
                If cs = Asc("$") OrElse cs = Asc("%") OrElse cs = Asc("&") _
                   OrElse cs = Asc("!") OrElse cs = Asc("#") Then
                    i += 1
                End If
            End If
            word = UCase(Mid(line_txt, start + 1, i - start))
            If is_keyword(word) Then
                For j = start To i - 1
                    colors(j) = COL_KEYWORD
                Next
                ' REM -> rest of line is a comment
                If word = "REM" Then
                    For j = i To n - 1
                        colors(j) = COL_COMMENT
                    Next
                    Exit Do
                End If
            Else
                For j = start To i - 1
                    colors(j) = COL_TEXT
                Next
            End If
            Continue Do
        End If

        ' --- Operator / punctuation ---
        If Instr("+-*/\^=<>(),;:.", Chr(c)) > 0 Then
            colors(i) = COL_OPERATOR
        Else
            colors(i) = COL_TEXT
        End If
        i += 1
    Loop
End Sub
