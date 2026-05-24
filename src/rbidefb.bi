' ============================================================================
'  rbidefb.bi -- shared types, constants, state, and includes
'  RB BASIC FreeBASIC IDE
' ============================================================================

#define VT_USE_TUI
#define VT_USE_STRINGS

' libvt must come first so VT_* constants are visible when we use them below.
#include once "vt/vt.bi"

Const RBIDEFB_VER = "0.10"

' ----------------------------------------------------------------------------
'  App-wide constants
' ----------------------------------------------------------------------------
Const APP_MIN_COLS = 100
Const APP_MIN_ROWS = 32

Const MENU_ROW         = 1
Const OUTPUT_MIN_ROWS  = 5
Const OUTPUT_FRAC_NUM  = 1
Const OUTPUT_FRAC_DEN  = 3

' Application views (switched with F2)
Const VIEW_EDITOR = 0
Const VIEW_CHAT   = 1

' DOS-style 16-color syntax-highlighting palette
Const COL_BG          = VT_BLUE
Const COL_TEXT        = VT_YELLOW
Const COL_KEYWORD     = VT_BRIGHT_CYAN
Const COL_STRING      = VT_BRIGHT_MAGENTA
Const COL_COMMENT     = VT_BRIGHT_GREEN
Const COL_NUMBER      = VT_BRIGHT_RED
Const COL_OPERATOR    = VT_WHITE
Const COL_LINENO      = VT_LIGHT_GREY
Const COL_LINENO_BG   = VT_BLACK

Const COL_SEL_BG      = VT_LIGHT_GREY
Const COL_SEL_FG      = VT_BLACK

Const COL_OUT_BG      = VT_BLACK
Const COL_OUT_FG      = VT_LIGHT_GREY
Const COL_OUT_ERR     = VT_BRIGHT_RED
Const COL_OUT_OK      = VT_BRIGHT_GREEN
Const COL_OUT_HDR     = VT_YELLOW

Const COL_STATUS_FG   = VT_BLACK
Const COL_STATUS_BG   = VT_LIGHT_GREY

Const COL_CHAT_BG     = VT_BLACK
Const COL_CHAT_USER   = VT_BRIGHT_CYAN
Const COL_CHAT_ASST   = VT_WHITE
Const COL_CHAT_SYS    = VT_YELLOW

Const EDITOR_MARGIN_W = 6
Const TAB_WIDTH       = 4

Const EDIT_INIT_CAPACITY  = 256
Const OUT_INIT_CAPACITY   = 512
Const CHAT_INIT_CAPACITY  = 64

' Menu group enum (1-based as VT_TUI returns)
Enum
    MNU_FILE = 1
    MNU_EDIT
    MNU_RUN
    MNU_VIEW
    MNU_HELP
End Enum

' ============================================================================
'  Editor state
' ============================================================================
ReDim Shared editLines(EDIT_INIT_CAPACITY - 1) As String
Dim Shared   editLineCount    As Long
Dim Shared   editCapacity     As Long
Dim Shared   editCurRow       As Long
Dim Shared   editCurCol       As Long
Dim Shared   editTopRow       As Long
Dim Shared   editLeftCol      As Long
Dim Shared   editSelActive    As Byte
Dim Shared   editSelAnchorRow As Long
Dim Shared   editSelAnchorCol As Long
Dim Shared   editDirty        As Byte
Dim Shared   editFilename     As String
Dim Shared   editClipboard    As String
Dim Shared   editFindLast     As String

' ============================================================================
'  Output panel state
' ============================================================================
ReDim Shared outLines(OUT_INIT_CAPACITY - 1)  As String
ReDim Shared outColors(OUT_INIT_CAPACITY - 1) As UByte
Dim Shared   outLineCount  As Long
Dim Shared   outCapacity   As Long
Dim Shared   outTopLine    As Long
Dim Shared   outFollowTail As Byte

' ============================================================================
'  Chat state
' ============================================================================
Type ChatMsg
    role As String
    txt  As String
End Type

ReDim Shared chatHistory(CHAT_INIT_CAPACITY - 1) As ChatMsg
Dim Shared   chatHistoryCount As Long
Dim Shared   chatHistoryCap   As Long
Dim Shared   chatInputEd      As vt_tui_editor_state
Dim Shared   chatScrollTop    As Long
Dim Shared   chatStreamAccum  As String
Dim Shared   chatStreaming    As Byte

' ============================================================================
'  Layout (recomputed every frame)
' ============================================================================
Dim Shared lytMenuRow      As Long
Dim Shared lytStatusRow    As Long
Dim Shared lytEditorY1     As Long
Dim Shared lytEditorY2     As Long
Dim Shared lytOutputY1     As Long
Dim Shared lytOutputY2     As Long
Dim Shared lytChatY1       As Long
Dim Shared lytChatY2       As Long
Dim Shared lytChatInputY1  As Long
Dim Shared lytChatInputY2  As Long
Dim Shared lytEdCols       As Long
Dim Shared lytEdRows       As Long
Dim Shared outSplitFrac    As Single

' ============================================================================
'  App-level state
' ============================================================================
Dim Shared appView        As Long
Dim Shared appRunning     As Byte
Dim Shared appFocusOutput As Byte
Dim Shared statusMsg      As String
Dim Shared statusMsgUntil As Double

' Settings
Dim Shared cfgApiKey     As String
Dim Shared cfgModel      As String
Dim Shared cfgMaxTokens  As Long
Dim Shared cfgSysPrompt  As String
Dim Shared cfgLastFile   As String
Dim Shared cfgScreenCols As Long
Dim Shared cfgScreenRows As Long

' Path to rbbasic.exe, discovered at startup
Dim Shared compilerPath  As String

' ============================================================================
'  fbclaude (Anthropic API client) -- self-contained module.
'  Pulled in from the existing vtclaude-main directory.
' ============================================================================
#include once "src/fbclaude.bas"

' ============================================================================
'  Forward declarations (cross-module)
' ============================================================================
Declare Sub status_set(msg As String, seconds As Double = 3.0)
Declare Sub out_append(line_txt As String, fg As UByte = COL_OUT_FG)
Declare Sub out_clear()
Declare Sub out_draw()
Declare Function out_handle(k As ULong) As Long

Declare Sub editor_init_empty()
Declare Sub editor_load_file(path As String)
Declare Function editor_save_file(path As String) As Long
Declare Sub editor_draw()
Declare Function editor_handle(k As ULong) As Long
Declare Sub editor_insert_text(s As String)
Declare Sub editor_cut()
Declare Sub editor_copy()
Declare Sub editor_paste()
Declare Sub editor_select_all()
Declare Sub editor_find_dialog()

Declare Sub syntax_tokenize_line(line_txt As String, colors() As UByte)

Declare Function compile_current(also_run As Byte) As Long
Declare Function run_program() As Long
Declare Sub compile_locate()

Declare Sub chat_init()
Declare Sub chat_shutdown()
Declare Sub chat_draw()
Declare Function chat_handle(k As ULong) As Long
Declare Sub chat_send_pending()
Declare Sub chat_clear()
Declare Sub chat_append(role As String, txt As String)

Declare Sub config_load()
Declare Sub config_save()

Declare Sub menu_build(groups() As String, items() As String, counts() As Long)
Declare Sub menu_dispatch(grp As Long, itm As Long)
Declare Sub menu_show_about()
Declare Sub menu_show_settings()
Declare Sub menu_file_open()
Declare Sub menu_file_save_as()
