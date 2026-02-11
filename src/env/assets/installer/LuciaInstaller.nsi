; ======================================================
; Lucia 2.0 Installer with LYM support
; ======================================================

!include MUI2.nsh
!include nsDialogs.nsh
!include LogicLib.nsh
!include FileFunc.nsh

!ifdef RELEASE_BUILD
  SetCompressor /SOLID /FINAL lzma
!endif

; -----------------------------
; Global Defines
!define APPNAME "Lucia"
!define VERSION "2.0.0"
!define LYMNAME "LYM (Lucia Package Manager)"

Name "${APPNAME} ${VERSION}"
OutFile "LuciaInstaller.exe"
InstallDir "$PROGRAMFILES\${APPNAME}"
RequestExecutionLevel admin
Icon "..\installer.ico"
SetOverwrite on

BrandingText "Installation Wizard by Markofwitch"

ShowInstDetails show
!define MUI_ABORTWARNING
!define MUI_ICON "..\installer.ico"
!define MUI_UNICON "..\uninstaller.ico"

; -----------------------------
; Vars (all)
Var Dialog

Var PATH_Checkbox
Var PATH_Enabled
Var LYM_Checkbox
Var LYM_Enabled
Var LYMAddToPath_Checkbox
Var LYMAddToPath_Enabled
Var AddLib_Header
Var WinLib_Header

Var Msgbox_Check
Var Sounds_Check
Var Clipboard_Check
Var Zip_Check
Var RAYLIB_Check
Var SQL_Check
Var Img_Check

Var Windows_Check
Var Win32_Check
Var COM_Check
Var DirectX_Check
Var Shell_Check
Var Shell_PS_Check
Var Hotkeys_Check
Var Services_Check

Var Msgbox_Enabled
Var Sounds_Enabled
Var Clipboard_Enabled
Var Zip_Enabled
Var RAYLIB_Enabled
Var SQL_Enabled
Var Img_Enabled

Var Windows_Enabled
Var Win32_Enabled
Var COM_Enabled
Var DirectX_Enabled
Var Shell_Enabled
Var Shell_PS_Enabled
Var Hotkeys_Enabled
Var Services_Enabled

Var LYM_PATH_HWND
Var LYM_PATH
Var LUCIA_PATH
Var ConfirmText
Var TestSuiteCheck
Var RunLuciaCheck

Var TmpVar1

Function .onInit
    StrCpy $PATH_Enabled ${BST_CHECKED}
    StrCpy $LYM_Enabled ${BST_CHECKED}
    StrCpy $LYMAddToPath_Enabled ${BST_CHECKED}
FunctionEnd

; -----------------------------
; Pages
!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_LICENSE "..\..\..\..\LICENSE"
Page custom OptionsPage OptionsPageLeave
Page custom LYMDirPage LYMDirPageLeave
Page custom ConfirmPage ConfirmPageLeave
!insertmacro MUI_PAGE_INSTFILES
Page custom FinishPage FinishPageLeave
!insertmacro MUI_LANGUAGE "English"

; ======================================================
; Options Page
Function OptionsPage
    nsDialogs::Create 1018
    Pop $Dialog

    ${NSD_CreateCheckbox} 20 20 300 12 "Add Lucia to PATH"
    Pop $PATH_Checkbox
    ${NSD_SetState} $PATH_Checkbox $PATH_Enabled

    ${NSD_CreateCheckbox} 20 50 300 12 "Install ${LYMNAME}"
    Pop $LYM_Checkbox
    ${NSD_SetState} $LYM_Checkbox $LYM_Enabled

    ${NSD_CreateCheckbox} 20 90 300 12 "Install Additional Libraries"
    Pop $AddLib_Header
    ${NSD_SetState} $AddLib_Header ${BST_UNCHECKED}

    ${NSD_CreateCheckbox} 40 110 180 18 "msgbox"
    Pop $Msgbox_Check
    ${NSD_SetState} $Msgbox_Check $Msgbox_Enabled

    ${NSD_CreateCheckbox} 40 130 180 18 "sounds"
    Pop $Sounds_Check
    ${NSD_SetState} $Sounds_Check $Sounds_Enabled

    ${NSD_CreateCheckbox} 40 150 180 18 "clipboard"
    Pop $Clipboard_Check
    ${NSD_SetState} $Clipboard_Check $Clipboard_Enabled

    ${NSD_CreateCheckbox} 40 170 180 18 "zip"
    Pop $Zip_Check
    ${NSD_SetState} $Zip_Check $Zip_Enabled

    ${NSD_CreateCheckbox} 260 110 200 18 "raylib"
    Pop $RAYLIB_Check
    ${NSD_SetState} $RAYLIB_Check $RAYLIB_Enabled

    ${NSD_CreateCheckbox} 260 130 200 18 "sql"
    Pop $SQL_Check
    ${NSD_SetState} $SQL_Check $SQL_Enabled

    ${NSD_CreateCheckbox} 260 150 220 18 "image"
    Pop $Img_Check
    ${NSD_SetState} $Img_Check $Img_Enabled

    ShowWindow $Msgbox_Check ${SW_HIDE}
    ShowWindow $Sounds_Check ${SW_HIDE}
    ShowWindow $Clipboard_Check ${SW_HIDE}
    ShowWindow $Zip_Check ${SW_HIDE}
    ShowWindow $RAYLIB_Check ${SW_HIDE}
    ShowWindow $SQL_Check ${SW_HIDE}
    ShowWindow $Img_Check ${SW_HIDE}

    ${NSD_OnClick} $AddLib_Header ToggleAddLib

    ${NSD_CreateCheckbox} 20 110 300 12 "Install Windows Specific Libraries"
    Pop $WinLib_Header
    ${NSD_SetState} $WinLib_Header ${BST_UNCHECKED}

    ${NSD_CreateCheckbox} 40 130 80 18 "Windows"
    Pop $Windows_Check
    ${NSD_SetState} $Windows_Check $Windows_Enabled

    ${NSD_CreateCheckbox} 40 150 60 18 "Win32"
    Pop $Win32_Check
    ${NSD_SetState} $Win32_Check $Win32_Enabled

    ${NSD_CreateCheckbox} 40 170 40 18 "COM"
    Pop $COM_Check
    ${NSD_SetState} $COM_Check $COM_Enabled

    ${NSD_CreateCheckbox} 40 190 70 18 "DirectX"
    Pop $DirectX_Check
    ${NSD_SetState} $DirectX_Check $DirectX_Enabled

    ${NSD_CreateCheckbox} 260 130 50 18 "Shell"
    Pop $Shell_Check
    ${NSD_SetState} $Shell_Check $Shell_Enabled

    ${NSD_CreateCheckbox} 260 150 200 18 "Enable PowerShell support"
    Pop $Shell_PS_Check
    ${NSD_SetState} $Shell_PS_Check $Shell_PS_Enabled

    ${NSD_CreateCheckbox} 260 170 70 18 "Hotkeys"
    Pop $Hotkeys_Check
    ${NSD_SetState} $Hotkeys_Check $Hotkeys_Enabled

    ${NSD_CreateCheckbox} 260 190 80 18 "Services"
    Pop $Services_Check
    ${NSD_SetState} $Services_Check $Services_Enabled

    ShowWindow $Windows_Check ${SW_HIDE}
    ShowWindow $Win32_Check ${SW_HIDE}
    ShowWindow $COM_Check ${SW_HIDE}
    ShowWindow $DirectX_Check ${SW_HIDE}
    ShowWindow $Shell_Check ${SW_HIDE}
    ShowWindow $Shell_PS_Check ${SW_HIDE}
    ShowWindow $Hotkeys_Check ${SW_HIDE}
    ShowWindow $Services_Check ${SW_HIDE}

    ${NSD_OnClick} $WinLib_Header ToggleWinLib

    nsDialogs::Show
FunctionEnd

; ======================================================
; Toggle for Additional Libraries
Function ToggleAddLib
    ${NSD_GetState} $AddLib_Header $0
    ${If} $0 = ${BST_CHECKED}
        ShowWindow $Msgbox_Check ${SW_SHOW}
        ShowWindow $Sounds_Check ${SW_SHOW}
        ShowWindow $Clipboard_Check ${SW_SHOW}
        ShowWindow $Zip_Check ${SW_SHOW}
        ShowWindow $RAYLIB_Check ${SW_SHOW}
        ShowWindow $SQL_Check ${SW_SHOW}
        ShowWindow $Img_Check ${SW_SHOW}
        ShowWindow $WinLib_Header ${SW_HIDE}
    ${Else}
        ShowWindow $Msgbox_Check ${SW_HIDE}
        ShowWindow $Sounds_Check ${SW_HIDE}
        ShowWindow $Clipboard_Check ${SW_HIDE}
        ShowWindow $Zip_Check ${SW_HIDE}
        ShowWindow $RAYLIB_Check ${SW_HIDE}
        ShowWindow $SQL_Check ${SW_HIDE}
        ShowWindow $Img_Check ${SW_HIDE}
        ShowWindow $WinLib_Header ${SW_SHOW}
    ${EndIf}
FunctionEnd

; ======================================================
; Toggle for Windows Libraries
Function ToggleWinLib
    ${NSD_GetState} $WinLib_Header $0
    ${If} $0 = ${BST_CHECKED}
        ShowWindow $Windows_Check ${SW_SHOW}
        ShowWindow $Win32_Check ${SW_SHOW}
        ShowWindow $COM_Check ${SW_SHOW}
        ShowWindow $DirectX_Check ${SW_SHOW}
        ShowWindow $Shell_Check ${SW_SHOW}
        ShowWindow $Shell_PS_Check ${SW_SHOW}
        ShowWindow $Hotkeys_Check ${SW_SHOW}
        ShowWindow $Services_Check ${SW_SHOW}
    ${Else}
        ShowWindow $Windows_Check ${SW_HIDE}
        ShowWindow $Win32_Check ${SW_HIDE}
        ShowWindow $COM_Check ${SW_HIDE}
        ShowWindow $DirectX_Check ${SW_HIDE}
        ShowWindow $Shell_Check ${SW_HIDE}
        ShowWindow $Shell_PS_Check ${SW_HIDE}
        ShowWindow $Hotkeys_Check ${SW_HIDE}
        ShowWindow $Services_Check ${SW_HIDE}
    ${EndIf}
FunctionEnd

; ======================================================
Function OptionsPageLeave
    ; Additional Libraries
    ${NSD_GetState} $Msgbox_Check $Msgbox_Enabled
    ${NSD_GetState} $Sounds_Check $Sounds_Enabled
    ${NSD_GetState} $Clipboard_Check $Clipboard_Enabled
    ${NSD_GetState} $Zip_Check $Zip_Enabled
    ${NSD_GetState} $RAYLIB_Check $RAYLIB_Enabled
    ${NSD_GetState} $SQL_Check $SQL_Enabled
    ${NSD_GetState} $Img_Check $Img_Enabled

    ; Windows Libraries
    ${NSD_GetState} $Windows_Check $Windows_Enabled
    ${NSD_GetState} $Win32_Check $Win32_Enabled
    ${NSD_GetState} $COM_Check $COM_Enabled
    ${NSD_GetState} $DirectX_Check $DirectX_Enabled
    ${NSD_GetState} $Shell_Check $Shell_Enabled
    ${NSD_GetState} $Shell_PS_Check $Shell_PS_Enabled
    ${NSD_GetState} $Hotkeys_Check $Hotkeys_Enabled
    ${NSD_GetState} $Services_Check $Services_Enabled

    ${NSD_GetState} $PATH_Checkbox $PATH_Enabled
    ${NSD_GetState} $LYM_Checkbox  $LYM_Enabled
    
    StrCpy $LUCIA_PATH "$INSTDIR\env\bin"
FunctionEnd

; ======================================================
; LYM Directory Page (asks for lym dir and Add to PATH)
Function LYMDirPage
    ${If} $LYM_Enabled = ${BST_CHECKED}
        nsDialogs::Create 1018
        Pop $Dialog
        ${NSD_CreateLabel} 0 0 100% 18 "Choose ${LYMNAME} installation directory:"
        ${NSD_CreateDirRequest} 0 25 100% 20 "$INSTDIR\lym"
        Pop $LYM_PATH_HWND
        ${NSD_CreateCheckbox} 0 60 250 18 "Add LYM to PATH"
        Pop $LYMAddToPath_Checkbox
        ${NSD_SetState} $LYMAddToPath_Checkbox $LYMAddToPath_Enabled
        nsDialogs::Show
    ${Else}
        Abort
    ${EndIf}
FunctionEnd

Function LYMDirPageLeave
    ${If} $LYM_Enabled = ${BST_CHECKED}
        ${NSD_GetText} $LYM_PATH_HWND $LYM_PATH
        ; ensure path is set
        ${If} $LYM_PATH == ""
            StrCpy $LYM_PATH "$INSTDIR\\lym"
        ${EndIf}
    ${Else}
        StrCpy $LYM_PATH ""
    ${EndIf}
    ${NSD_GetState} $LYMAddToPath_Checkbox $LYMAddToPath_Enabled
FunctionEnd

; ======================================================
; Confirm Page - displays chosen options
Function ConfirmPage
    nsDialogs::Create 1018
    Pop $Dialog
    StrCpy $ConfirmText "You have chosen the following settings:$\r$\n$\r$\n"

    ; -------------------------------
    ; Lucia PATH
    ${If} $PATH_Enabled == ${BST_CHECKED}
        StrCpy $ConfirmText "$ConfirmText- Add Lucia to PATH: Yes$\r$\n"
    ${Else}
        StrCpy $ConfirmText "$ConfirmText- Add Lucia to PATH: No$\r$\n"
    ${EndIf}

    ; -------------------------------
    ; LYM
    ${If} $LYM_Enabled == ${BST_CHECKED}
        StrCpy $ConfirmText "$ConfirmText- Install ${LYMNAME}: Yes$\r$\n  - Dir: $LYM_PATH$\r$\n"
        ${If} $LYMAddToPath_Enabled == ${BST_CHECKED}
            StrCpy $ConfirmText "$ConfirmText  - Add to PATH: Yes$\r$\n"
        ${Else}
            StrCpy $ConfirmText "$ConfirmText  - Add to PATH: No$\r$\n"
        ${EndIf}
    ${Else}
        StrCpy $ConfirmText "$ConfirmText- Install ${LYMNAME}: No$\r$\n"
    ${EndIf}

    ; -------------------------------
    ; Additional Libraries
    StrCpy $TmpVar1 0

    ${If} $Msgbox_Enabled == ${BST_CHECKED}
        StrCpy $TmpVar1 1
    ${EndIf}

    ${If} $Sounds_Enabled == ${BST_CHECKED}
        StrCpy $TmpVar1 1
    ${EndIf}

    ${If} $Clipboard_Enabled == ${BST_CHECKED}
        StrCpy $TmpVar1 1
    ${EndIf}

    ${If} $Zip_Enabled == ${BST_CHECKED}
        StrCpy $TmpVar1 1
    ${EndIf}

    ${If} $RAYLIB_Enabled == ${BST_CHECKED}
        StrCpy $TmpVar1 1
    ${EndIf}

    ${If} $SQL_Enabled == ${BST_CHECKED}
        StrCpy $TmpVar1 1
    ${EndIf}

    ${If} $Img_Enabled == ${BST_CHECKED}
        StrCpy $TmpVar1 1
    ${EndIf}

    ${If} $TmpVar1 == 1
        StrCpy $ConfirmText "$ConfirmText- Additional Libraries:$\r$\n"

        ${If} $Msgbox_Enabled == ${BST_CHECKED}
            StrCpy $ConfirmText "$ConfirmText  - msgbox$\r$\n"
        ${EndIf}

        ${If} $Sounds_Enabled == ${BST_CHECKED}
            StrCpy $ConfirmText "$ConfirmText  - sounds$\r$\n"
        ${EndIf}

        ${If} $Clipboard_Enabled == ${BST_CHECKED}
            StrCpy $ConfirmText "$ConfirmText  - clipboard$\r$\n"
        ${EndIf}

        ${If} $Zip_Enabled == ${BST_CHECKED}
            StrCpy $ConfirmText "$ConfirmText  - zip$\r$\n"
        ${EndIf}

        ${If} $RAYLIB_Enabled == ${BST_CHECKED}
            StrCpy $ConfirmText "$ConfirmText  - raylib$\r$\n"
        ${EndIf}

        ${If} $SQL_Enabled == ${BST_CHECKED}
            StrCpy $ConfirmText "$ConfirmText  - sql$\r$\n"
        ${EndIf}

        ${If} $Img_Enabled == ${BST_CHECKED}
            StrCpy $ConfirmText "$ConfirmText  - image$\r$\n"
        ${EndIf}
    ${Else}
        StrCpy $ConfirmText "$ConfirmText- Additional Libraries: None$\r$\n"
    ${EndIf}

    ; -------------------------------
    ; Windows Libraries
    StrCpy $TmpVar1 0

    ${If} $Windows_Enabled == ${BST_CHECKED}
        StrCpy $TmpVar1 1
    ${EndIf}

    ${If} $Win32_Enabled == ${BST_CHECKED}
        StrCpy $TmpVar1 1
    ${EndIf}

    ${If} $COM_Enabled == ${BST_CHECKED}
        StrCpy $TmpVar1 1
    ${EndIf}

    ${If} $DirectX_Enabled == ${BST_CHECKED}
        StrCpy $TmpVar1 1
    ${EndIf}

    ${If} $Shell_Enabled == ${BST_CHECKED}
        StrCpy $TmpVar1 1
    ${EndIf}

    ${If} $Shell_PS_Enabled == ${BST_CHECKED}
        StrCpy $TmpVar1 1
    ${EndIf}

    ${If} $Hotkeys_Enabled == ${BST_CHECKED}
        StrCpy $TmpVar1 1
    ${EndIf}

    ${If} $Services_Enabled == ${BST_CHECKED}
        StrCpy $TmpVar1 1
    ${EndIf}

    ${If} $TmpVar1 == 1
        StrCpy $ConfirmText "$ConfirmText- Windows Libraries:$\r$\n"

        ${If} $Windows_Enabled == ${BST_CHECKED}
            StrCpy $ConfirmText "$ConfirmText  - Windows$\r$\n"
        ${EndIf}

        ${If} $Win32_Enabled == ${BST_CHECKED}
            StrCpy $ConfirmText "$ConfirmText  - Win32$\r$\n"
        ${EndIf}

        ${If} $COM_Enabled == ${BST_CHECKED}
            StrCpy $ConfirmText "$ConfirmText  - COM$\r$\n"
        ${EndIf}

        ${If} $DirectX_Enabled == ${BST_CHECKED}
            StrCpy $ConfirmText "$ConfirmText  - DirectX$\r$\n"
        ${EndIf}

        ${If} $Shell_Enabled == ${BST_CHECKED}
            StrCpy $ConfirmText "$ConfirmText  - Shell"
            ${If} $Shell_PS_Enabled == ${BST_CHECKED}
                StrCpy $ConfirmText "$ConfirmText  - PowerShell$\r$\n"
            ${Else}
                StrCpy $ConfirmText "$ConfirmText$\r$\n"
            ${EndIf}
        ${EndIf}

        ${If} $Hotkeys_Enabled == ${BST_CHECKED}
            StrCpy $ConfirmText "$ConfirmText  - Hotkeys$\r$\n"
        ${EndIf}

        ${If} $Services_Enabled == ${BST_CHECKED}
            StrCpy $ConfirmText "$ConfirmText  - Services$\r$\n"
        ${EndIf}
    ${Else}
        StrCpy $ConfirmText "$ConfirmText- Windows Libraries: None$\r$\n"
    ${EndIf}

    ; -------------------------------
    ; Final prompt
    StrCpy $ConfirmText "$ConfirmText$\r$\nDo you want to install ${APPNAME} ${VERSION} with these settings?"

    ; Show the summary
    ${NSD_CreateLabel} 0 0 100% 200 "$ConfirmText"
    nsDialogs::Show
FunctionEnd

Function ConfirmPageLeave
FunctionEnd

; ======================================================
; Finish Page - create dialog
Function FinishPage
    nsDialogs::Create 1018
    Pop $Dialog

    ; Title label
    ${NSD_CreateLabel} 0 0 100% 12 "Installation completed."

    ; Checkbox: Run Lucia test suite
    ${NSD_CreateCheckbox} 0 30 100% 12 "Run Lucia test suite after close"
    Pop $TestSuiteCheck

    ; Checkbox: Run Lucia
    ${NSD_CreateCheckbox} 0 50 100% 12 "Run Lucia after close"
    Pop $RunLuciaCheck
    ${NSD_SetState} $RunLuciaCheck ${BST_CHECKED}

    nsDialogs::Show
FunctionEnd

Function FinishPageLeave
    ; Run Lucia if checked
    ${NSD_GetState} $RunLuciaCheck $TmpVar1
    ${If} $TmpVar1 = ${BST_CHECKED}
        Exec '"$INSTDIR\src\env\bin\lucia.exe"'
    ${EndIf}

    ; Run test suite if checked
    ${NSD_GetState} $TestSuiteCheck $TmpVar1
    ${If} $TmpVar1 = ${BST_CHECKED}
        SetOutPath "$INSTDIR\tests"
        Exec '"$INSTDIR\tests\run_tests.exe"'
    ${EndIf}
FunctionEnd

Function InstallLibrary
    Exch $0
    nsExec::ExecToStack '"$LYM_PATH\bin\lym.exe" install $0 --no-confirm'
    Pop $1
    DetailPrint "Installed $0"
FunctionEnd

; ======================================================
; Helper: Add folder to PATH only if not already present
Function AddToPath
    Exch $0
    ReadRegStr $1 HKLM "SYSTEM\CurrentControlSet\Control\Session Manager\Environment" "Path"

    StrCpy $2 $1
    loop:
        StrCpy $3 $2 1 0
        StrCmp $3 "" done
        StrCmp $2 "$0" found
        StrCpy $2 $2 "" 1
        Goto loop

    found:
        DetailPrint "$0 already in PATH, skipping"
        Return

    done:
        StrCpy $1 "$0;$1"
        WriteRegStr HKLM "SYSTEM\CurrentControlSet\Control\Session Manager\Environment" "Path" "$1"
        DetailPrint "Added $0 to PATH"
FunctionEnd

; ======================================================
; Installation Section
Section "Install"

    RMDir /r "$INSTDIR"
    CreateDirectory "$INSTDIR"

    SetOutPath "$INSTDIR"

    ; Root files
    File "..\..\..\..\README.md"
    File "..\..\..\..\LICENSE"

    ; src files
    SetOutPath "$INSTDIR\src\"
    File /r "..\..\..\..\src\main.rs"
    File /r "..\..\..\..\src\lexer.rs"
    File /r "..\..\..\..\src\parser.rs"
    File /r "..\..\..\..\src\interpreter.rs"

    ; env files
    SetOutPath "$INSTDIR\src\env\"
    File /r "..\..\..\..\src\env\config.json"
    File /r "..\..\..\..\src\env\libs.json"
    SetOutPath "$INSTDIR\src\env\bin"
    File /r "..\..\..\..\src\env\bin\*"
    SetOutPath "$INSTDIR\src\env\bundler"
    File /r "..\..\..\..\src\env\bundler\*"
    SetOutPath "$INSTDIR\src\env\Docs"
    File /r "..\..\..\..\src\env\Docs\*"
    SetOutPath "$INSTDIR\src\env\runtime"
    File /r "..\..\..\..\src\env\runtime\*"
    SetOutPath "$INSTDIR\src\env\libs"
    File /r "..\..\..\..\src\env\libs\*"

    ; SetOutPath "$INSTDIR\src\env\libs"

    ; ; libs files
    ; File "..\..\..\..\src\env\libs\*"

    ; FindFirst $0 $1 "..\..\..\..\src\env\libs\*"

    ; libs_loop:
    ;     StrCmp $1 "" libs_done

    ;     IfFileExists "..\..\..\..\src\env\libs\$1\*.*" 0 libs_next

    ;     StrCmp $1 "raylib_lucia" libs_next

    ;     File /r "..\..\..\..\src\env\libs\$1\*"

    ; libs_next:
    ;     FindNext $0 $1
    ;     Goto libs_loop

    ; libs_done:
    ; FindClose $0

    ; assets
    SetOutPath "$INSTDIR\src\env\assets\installer"
    File /r "..\..\..\..\src\env\assets\installer\*"
    SetOutPath "$INSTDIR\src\env\assets"
    File "..\..\..\..\src\env\assets\logo_lucia.png"
    File "..\..\..\..\src\env\assets\lucia_icon.ico"
    File "..\..\..\..\src\env\assets\lucia-syntax.vsix"

    ; tests
    SetOutPath "$INSTDIR\tests"
    File /r "..\..\..\..\tests\*"

    CreateDirectory "$INSTDIR\src\env\bin"
    WriteUninstaller "$INSTDIR\src\env\bin\uninstall_lucia.exe"

    ${If} $PATH_Enabled = ${BST_CHECKED}
        Push "$INSTDIR\src\env\bin"
        Call AddToPath
        Push "$INSTDIR\src\env\assets\include"
        Call AddToPath
    ${EndIf}

    ${If} $LYM_Enabled = ${BST_CHECKED}
        CreateDirectory "$LYM_PATH"
        SetOutPath "$LYM_PATH\bin"
        File /r "..\..\..\..\src\env\assets\lym\bin\*"
        SetOutPath "$LYM_PATH\src"
        File /r "..\..\..\..\src\env\assets\lym\src\*"
        SetOutPath "$LYM_PATH\"
        File /r "..\..\..\..\src\env\assets\lym\README.md"

        ${If} $LYMAddToPath_Enabled = ${BST_CHECKED}
            Push "$LYM_PATH\bin"
            Call AddToPath
        ${EndIf}

        nsExec::ExecToStack '"$LYM_PATH\bin\lym.exe" config fetch' $R0
        DetailPrint "Fetched lym config."

        ; install selected libraries via lym
        ${If} $Msgbox_Enabled == ${BST_CHECKED}
            Push "msgbox"
            Call InstallLibrary
        ${EndIf}
        ${If} $Sounds_Enabled == ${BST_CHECKED}
            Push "sounds"
            Call InstallLibrary
        ${EndIf}
        ${If} $Clipboard_Enabled == ${BST_CHECKED}
            Push "clipboard"
            Call InstallLibrary
        ${EndIf}
        ${If} $Zip_Enabled == ${BST_CHECKED}
            Push "zip"
            Call InstallLibrary
        ${EndIf}
        ${If} $RAYLIB_Enabled == ${BST_CHECKED}
            Push "raylib_lucia"
            Call InstallLibrary
        ${EndIf}
        ${If} $SQL_Enabled == ${BST_CHECKED}
            Push "sql"
            Call InstallLibrary
        ${EndIf}
        ${If} $Img_Enabled == ${BST_CHECKED}
            Push "image"
            Call InstallLibrary
        ${EndIf}

        ; Windows specific
        ${If} $Windows_Enabled == ${BST_CHECKED}
            Push "Windows"
            Call InstallLibrary
        ${EndIf}
        ${If} $Win32_Enabled == ${BST_CHECKED}
            Push "Win32"
            Call InstallLibrary
        ${EndIf}
        ${If} $COM_Enabled == ${BST_CHECKED}
            Push "COM"
            Call InstallLibrary
        ${EndIf}
        ${If} $DirectX_Enabled == ${BST_CHECKED}
            Push "DirectX"
            Call InstallLibrary
        ${EndIf}
        ${If} $Shell_Enabled == ${BST_CHECKED}
            Push "Shell"
            Call InstallLibrary
            ${If} $Shell_PS_Enabled == ${BST_CHECKED}
                Push "shell-powershell"
                Call InstallLibrary
            ${EndIf}
        ${EndIf}
        ${If} $Hotkeys_Enabled == ${BST_CHECKED}
            Push "Hotkeys"
            Call InstallLibrary
        ${EndIf}
        ${If} $Services_Enabled == ${BST_CHECKED}
            Push "Services"
            Call InstallLibrary
        ${EndIf}
    ${EndIf}

    nsExec::ExecToStack '"$INSTDIR\src\env\bin\lucia.exe" -a -e' $R0
    DetailPrint "Updated lucia environment after install."

SectionEnd

; ======================================================
; Uninstallation Section
Section "Uninstall"

    ReadRegStr $0 HKLM "SYSTEM\CurrentControlSet\Control\Session Manager\Environment" "Path"
    StrCpy $1 ""
    StrCpy $2 $0

    loop_lucia:
        StrCpy $3 $2 ";"
        StrCmp $2 "" done_lucia

        StrCmp $3 "$INSTDIR\src\env\bin" skip_lucia
        StrCpy $1 "$1$3;"
        skip_lucia:
        StrCpy $2 $2 "" 1
        Goto loop_lucia
    done_lucia:
        WriteRegStr HKLM "SYSTEM\CurrentControlSet\Control\Session Manager\Environment" "Path" "$1"
        DetailPrint "Removed $INSTDIR\src\env\bin from PATH"

    ${If} $LYM_Enabled = ${BST_CHECKED}
        ReadRegStr $0 HKLM "SYSTEM\CurrentControlSet\Control\Session Manager\Environment" "Path"
        StrCpy $1 ""
        StrCpy $2 $0

        loop_lym:
            StrCpy $3 $2 ";"
            StrCmp $2 "" done_lym

            StrCmp $3 "$LYM_PATH\bin" skip_lym
            StrCpy $1 "$1$3;"
            skip_lym:
            StrCpy $2 $2 "" 1
            Goto loop_lym
        done_lym:
            WriteRegStr HKLM "SYSTEM\CurrentControlSet\Control\Session Manager\Environment" "Path" "$1"
            DetailPrint "Removed $LYM_PATH\bin from PATH"
    ${EndIf}

    RMDir /r "$INSTDIR"

    DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\App Paths\lucia.exe"
    ${If} $LYM_Enabled = ${BST_CHECKED}
        DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\App Paths\lym.exe"
    ${EndIf}

SectionEnd
