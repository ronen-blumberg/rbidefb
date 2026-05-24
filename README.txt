==============================================================================
  rbidefb - RB BASIC IDE (FreeBASIC edition)
==============================================================================

A second IDE for the RB-BASIC compiler, written in FreeBASIC using the
libvt text-mode UI library.  Companion to the C++/Scintilla "rbide".

  * Resizable text-mode window (libvt + SDL2 backend)
  * QB-style 16-color DOS syntax highlighting
  * Built-in Claude chat assistant (Anthropic API via libcurl)
  * Editor + Output panel layout like QB64
  * Menubar like vtirc / vtclaude-chat

------------------------------------------------------------------------------
  REQUIREMENTS
------------------------------------------------------------------------------

  * FreeBASIC 1.10.1 or newer (32-bit, fbc32.exe).
    Auto-detected at:
      C:\fb_programming\FreeBASIC-1.10.1-winlibs-gcc-9.3.0\
      C:\FreeBASIC\
      C:\Program Files\FreeBASIC\
    Or anywhere on PATH.

  * libvt headers + sources installed under FreeBASIC's inc\vt\ folder.
    (Already present in the included FB install.)

  * libcurl.dll + curl-ca-bundle.crt next to rbidefb.exe (for chat).
    build.bat copies these from libvt-examples\vtclaude-main\ automatically.

  * SDL2.dll next to rbidefb.exe (for libvt rendering).
    build.bat copies it from the rbbasic-portable root.

  * MinGW32 + rbbasic.exe one level up (rbbasic-portable\rbbasic.exe).

------------------------------------------------------------------------------
  BUILDING
------------------------------------------------------------------------------

    cd rbidefb
    build.bat

This produces rbidefb.exe in the rbidefb\ folder along with the runtime DLLs.

------------------------------------------------------------------------------
  RUNNING
------------------------------------------------------------------------------

    rbidefb.exe

The first time you launch it, set your Claude API key under
Help -> Settings.  Get one at https://console.anthropic.com .

------------------------------------------------------------------------------
  KEYBOARD
------------------------------------------------------------------------------

  Global
  ---------------------------------------------------------------------------
  F1                  About
  F2                  Toggle Editor / Chat view
  F4                  Toggle focus to Output panel (PgUp/PgDn scrolls)
  F5                  Compile and Run
  F7                  Compile
  F9                  Run last build
  Alt + F/E/R/V/H     Open menu group
  Ctrl+N              New file
  Ctrl+O              Open file
  Ctrl+S              Save
  Ctrl+Q              Quit

  Editor view
  ---------------------------------------------------------------------------
  Arrow keys          Move cursor
  Ctrl + Left/Right   Word jump
  Home / End          Line start / end (Home toggles col 0 / first non-ws)
  Ctrl + Home/End     Buffer start / end
  PgUp / PgDn         Page up / down
  Shift + arrows      Extend selection
  Ctrl+X / C / V      Cut / Copy / Paste
  Ctrl+A              Select all
  Ctrl+F              Find
  Tab                 Insert spaces to next tab stop (TAB_WIDTH = 4)
  Backspace / Delete  Self-explanatory
  Enter               Newline with auto-indent

  Chat view
  ---------------------------------------------------------------------------
  Ctrl+Enter          Send message
  Ctrl+L              Clear chat history
  PgUp / PgDn         Scroll history

------------------------------------------------------------------------------
  CONFIGURATION
------------------------------------------------------------------------------

Settings live in rbidefb.cfg next to the exe:

    APIKEY=sk-ant-...
    MODEL=claude-haiku-4-5
    MAXTOKENS=2048
    SYSPROMPT=You are a helpful coding assistant...
    LASTFILE=C:\path\to\last.bas
    SCREENCOLS=120
    SCREENROWS=40

Available models:
    claude-haiku-4-5    (fast, cheap; recommended default)
    claude-sonnet-4-6   (balanced)
    claude-opus-4-7     (best, slowest)

------------------------------------------------------------------------------
  PROJECT LAYOUT
------------------------------------------------------------------------------

    rbidefb\
      build.bat            -- compile script
      README.txt           -- this file
      rbidefb.exe          -- after build
      libcurl.dll          -- copied from vtclaude-main
      curl-ca-bundle.crt   -- copied from vtclaude-main
      SDL2.dll             -- copied from rbbasic-portable root
      rbidefb.cfg          -- created on first quit
      src\
        rbidefb.bas        -- main entry + event loop
        rbidefb.bi         -- shared types, constants, state, includes
        syntax.bas         -- RB-BASIC tokenizer + keyword table
        editor.bas         -- custom syntax-highlight editor widget
        output.bas         -- output panel
        compiler.bas       -- drives rbbasic.exe + compile.bat
        chat.bas           -- Claude chat panel (uses fbclaude.bas)
        config.bas         -- load/save rbidefb.cfg
        menu.bas           -- menubar build + dispatch + dialogs

------------------------------------------------------------------------------
  RELATIONSHIP TO OTHER COMPONENTS
------------------------------------------------------------------------------

  ../rbbasic.exe          The compiler.  Invoked as a child process.
  ../libvt-main           libvt source + docs.  Headers used during build.
  ../libvt-examples/
      vtclaude-main       Source of fbclaude.bas (re-included from here);
                          also source of libcurl.dll + curl-ca-bundle.crt.
      vtirc-main          Menubar pattern reference (not linked at runtime).
  ../rbide                The original C++ IDE (Scintilla / Win32).
                          rbidefb is an independent FreeBASIC alternative;
                          both share the same rbbasic.exe compiler.

==============================================================================
