' ============================================================================
'  compiler.bas -- drive rbbasic.exe + compile.bat from the IDE.
'
'  Pipeline (matches what the C++ rbide does):
'    1. Save the editor buffer to disk.
'    2. CD to the source-file directory.
'    3. Run "rbbasic.exe <file.bas>" with stdout/stderr captured.
'    4. If exit=0 and compile.bat appeared, run "compile.bat".
'    5. If also_run, launch the resulting program.exe asynchronously.
' ============================================================================

' ----------------------------------------------------------------------------
'  Locate rbbasic.exe.  We're usually invoked as
'      rbbasic-portable\rbidefb\rbidefb.exe
'  so the compiler lives at  ..\rbbasic.exe  relative to ExePath().
' ----------------------------------------------------------------------------
Sub compile_locate()
    Dim As String candidates(2)
    candidates(0) = ExePath() & "\..\rbbasic.exe"
    candidates(1) = ExePath() & "\rbbasic.exe"
    candidates(2) = CurDir() & "\rbbasic.exe"

    Dim As Integer i, fh
    For i = 0 To 2
        fh = FreeFile
        If Open(candidates(i) For Input As #fh) = 0 Then
            Close #fh
            compilerPath = candidates(i)
            Exit Sub
        End If
    Next

    compilerPath = "rbbasic.exe"   ' fall back to PATH lookup
End Sub

' ----------------------------------------------------------------------------
'  Read a file as a single String (newlines preserved).
' ----------------------------------------------------------------------------
Private Function slurp_file(path As String) As String
    Dim As Integer fh = FreeFile
    If Open(path For Binary Access Read As #fh) <> 0 Then Return ""
    Dim As String buf
    Dim As Long n = Lof(fh)
    If n > 0 Then
        buf = String(n, 0)
        Get #fh, , buf
    End If
    Close #fh
    Return buf
End Function

' ----------------------------------------------------------------------------
'  Split a path into (dir, base).  dir keeps no trailing slash.
' ----------------------------------------------------------------------------
Private Sub split_path(path As String, ByRef dir_part As String, ByRef base_part As String)
    Dim As Long i
    For i = Len(path) To 1 Step -1
        Dim As Integer c = path[i - 1]
        If c = Asc("\") OrElse c = Asc("/") Then
            dir_part  = Left(path, i - 1)
            base_part = Mid(path, i + 1)
            Exit Sub
        End If
    Next
    dir_part  = ""
    base_part = path
End Sub

' ----------------------------------------------------------------------------
'  Append the contents of out.tmp / err.tmp under appropriate colors.
' ----------------------------------------------------------------------------
Private Sub append_temp_file(path As String, fg As UByte)
    Dim As String s = slurp_file(path)
    If Len(s) = 0 Then Exit Sub
    out_append(s, fg)
End Sub

' ----------------------------------------------------------------------------
'  Run a shell command, redirecting stdout/stderr to temp files, then dump
'  the captured output into the output panel.  Returns the process exit code.
'
'  We can't just call Shell(cmd_line & " >out 2>err") because FB's Shell goes
'  via cmd /c "<our string>" and cmd's special "/c with quotes" parsing strips
'  the outer pair when the inner string contains nested quotes AND a redirect.
'  That mangles the executable path.  Writing the command to a .bat file and
'  shelling THAT bypasses the issue entirely.
' ----------------------------------------------------------------------------
Private Function run_capture(workdir As String, cmd_line As String) As Long
    Dim As String tmp_out = workdir & "\rbidefb_stdout.tmp"
    Dim As String tmp_err = workdir & "\rbidefb_stderr.tmp"
    Dim As String tmp_bat = workdir & "\rbidefb_run.bat"

    Dim As Integer fh = FreeFile
    If Open(tmp_bat For Output As #fh) <> 0 Then
        out_append("Cannot create temp script: " & tmp_bat, COL_OUT_ERR)
        Return -1
    End If
    Print #fh, "@echo off"
    Print #fh, cmd_line & " >""" & tmp_out & """ 2>""" & tmp_err & """"
    Close #fh

    Dim As String old_cd = CurDir()
    ChDir workdir
    Dim As Long rc = Shell(tmp_bat)
    ChDir old_cd

    append_temp_file(tmp_out, COL_OUT_FG)
    append_temp_file(tmp_err, COL_OUT_ERR)

    Kill tmp_out
    Kill tmp_err
    Kill tmp_bat

    Return rc
End Function

' ----------------------------------------------------------------------------
'  Save the buffer if dirty / untitled.
'  Returns 0 on success, 1 if user cancelled or save failed.
' ----------------------------------------------------------------------------
Private Function ensure_saved() As Long
    If Len(editFilename) = 0 Then
        Dim As String pick = vt_tui_file_dialog("Save As", CurDir() & "/", "*.bas")
        If Len(pick) = 0 Then Return 1
        ' Normalize slashes to backslashes on Windows
        Dim As Long i
        For i = 1 To Len(pick)
            If pick[i - 1] = Asc("/") Then pick[i - 1] = Asc("\")
        Next
        If editor_save_file(pick) <> 0 Then Return 1
    ElseIf editDirty Then
        If editor_save_file(editFilename) <> 0 Then Return 1
    End If
    Return 0
End Function

' ----------------------------------------------------------------------------
'  compile_current
'  Returns 0 on success, non-zero on any failure.
'  When also_run is non-zero and compilation succeeds, fires the program
'  asynchronously via "start".
' ----------------------------------------------------------------------------
Function compile_current(also_run As Byte) As Long
    If Len(compilerPath) = 0 Then compile_locate()

    If ensure_saved() <> 0 Then
        status_set "Compile cancelled", 2.0
        Return 1
    End If

    Dim As String src_dir, src_base
    split_path(editFilename, src_dir, src_base)
    If Len(src_dir) = 0 Then src_dir = CurDir()

    out_clear()
    outFollowTail = 1
    out_append("=== Compile: " & src_base & " ===", COL_OUT_HDR)

    Dim As String step1 = """" & compilerPath & """ """ & src_base & """"
    Dim As Long rc1 = run_capture(src_dir, step1)

    If rc1 <> 0 Then
        out_append("--- rbbasic failed (exit " & rc1 & ") ---", COL_OUT_ERR)
        status_set "Compile failed", 4.0
        Return 2
    End If

    ' compile.bat is generated by rbbasic.  Run it.
    Dim As String bat_path = src_dir & "\compile.bat"
    Dim As Integer fh = FreeFile
    If Open(bat_path For Input As #fh) <> 0 Then
        out_append("(no compile.bat generated)", COL_OUT_ERR)
        status_set "Compile produced no compile.bat", 4.0
        Return 3
    End If
    Close #fh

    out_append("--- Linking via compile.bat ---", COL_OUT_HDR)
    Dim As Long rc2 = run_capture(src_dir, """" & bat_path & """")

    If rc2 <> 0 Then
        out_append("--- compile.bat failed (exit " & rc2 & ") ---", COL_OUT_ERR)
        status_set "Link failed", 4.0
        Return 4
    End If

    out_append("--- BUILD OK ---", COL_OUT_OK)
    status_set "Build succeeded", 3.0

    If also_run Then
        run_program()
    End If

    Return 0
End Function

' ----------------------------------------------------------------------------
'  Launch the compiled program.exe asynchronously.
'  rbbasic places the executable in the same directory as the source.
' ----------------------------------------------------------------------------
Function run_program() As Long
    Dim As String src_dir, src_base
    If Len(editFilename) = 0 Then
        status_set "Nothing to run (file is untitled)", 3.0
        Return 1
    End If
    split_path(editFilename, src_dir, src_base)
    If Len(src_dir) = 0 Then src_dir = CurDir()

    Dim As String exe = src_dir & "\program.exe"
    Dim As Integer fh = FreeFile
    If Open(exe For Input As #fh) <> 0 Then
        out_append("Cannot find: " & exe, COL_OUT_ERR)
        status_set "Run failed -- compile first", 3.0
        Return 2
    End If
    Close #fh

    out_append("=== Running " & exe & " ===", COL_OUT_HDR)
    status_set "Launched program.exe", 2.0

    ' "start" returns immediately, so the IDE stays responsive.
    Dim As String old_cd = CurDir()
    ChDir src_dir
    Shell "cmd /c start """" """ & exe & """"
    ChDir old_cd
    Return 0
End Function
