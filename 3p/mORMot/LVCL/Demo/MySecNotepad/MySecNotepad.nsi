; basic script template for NSIS installers
;
; Written by Philip Chu
; Copyright (c) 2004-2005 Technicat, LLC
;
; This software is provided 'as-is', without any express or implied warranty.
; In no event will the authors be held liable for any damages arising from the use of this software.
 
; Permission is granted to anyone to use this software for any purpose,
; including commercial applications, and to alter it ; and redistribute
; it freely, subject to the following restrictions:
 
;    1. The origin of this software must not be misrepresented; you must not claim that
;       you wrote the original software. If you use this software in a product, an
;       acknowledgment in the product documentation would be appreciated but is not required.
 
;    2. Altered source versions must be plainly marked as such, and must not be
;       misrepresented as being the original software.
 
;    3. This notice may not be removed or altered from any source distribution.

!packhdr "$%TEMP%\exehead.tmp" "upx2 -9 $%TEMP%\exehead.tmp"
;  pack the header even more thanks to upx :)

LoadLanguageFile "${NSISDIR}\Contrib\Language files\French.nlf"
LoadLanguageFile "${NSISDIR}\Contrib\Language files\English.nlf"

!define setup "MySecNotepad100Setup.exe"
 
; change this to wherever the files to be packaged reside
!define srcdir "."
 
!define company "Synopse"
 
!define prodname "My Secure Notepad"

LangString prodname ${LANG_ENGLISH} "My Secure Notepad"
LangString prodname ${LANG_FRENCH} "Mon Bloc-notes chiffré"

!define exec "MySecNotepad.exe"
 
; optional stuff
 
; text file to open in notepad after installation
; !define notefile "README.txt"
 
; license text file
; !define licensefile license.txt
 
; icons must be Microsoft .ICO files
!define icon "MySecNotepad.ico"

!define filetype ".crypt"
!define website "http://bouchez.info"

; installer background screen
; !define screenimage background.bmp
 
; file containing list of file-installation commands
; !define files "files.nsi"
 
; file containing list of file-uninstall commands
; !define unfiles "unfiles.nsi"

SetCompressor /SOLID lzma
 
; registry stuff
 
!define regkey "Software\${company}\${prodname}"
!define uninstkey "Software\Microsoft\Windows\CurrentVersion\Uninstall\$(prodname)"
 
;!define startmenu "$SMPROGRAMS\${company}\${prodname}"
!define startmenu "$SMPROGRAMS\$(prodname)"
!define uninstaller "uninstall.exe"
 
;--------------------------------
 
XPStyle on
ShowInstDetails hide
ShowUninstDetails hide
 
Name "$(prodname)"
Caption "$(prodname)"
 
!ifdef icon
Icon "${icon}"
!endif
 
OutFile "${setup}"
 
SetDateSave on
SetDatablockOptimize on
CRCCheck on
SilentInstall normal
 
;InstallDir "$PROGRAMFILES\${company}\${prodname}"
InstallDir "$PROGRAMFILES\${prodname}"
InstallDirRegKey HKLM "${regkey}" ""
 
!ifdef licensefile
LicenseText "License"
LicenseData "${srcdir}\${licensefile}"
!endif
 
; pages
; we keep it simple - leave out selectable installation types
 
!ifdef licensefile
Page license
!endif
 
; Page components
Page directory
Page instfiles
 
UninstPage uninstConfirm
UninstPage instfiles
 
;--------------------------------
 
AutoCloseWindow false
ShowInstDetails show
 
 
!ifdef screenimage
 
; set up background image
; uses BgImage plugin
 
Function .onGUIInit
	; extract background BMP into temp plugin directory
	InitPluginsDir
	File /oname=$PLUGINSDIR\1.bmp "${screenimage}"
 
	BgImage::SetBg /NOUNLOAD /FILLSCREEN $PLUGINSDIR\1.bmp
	BgImage::Redraw /NOUNLOAD
FunctionEnd
 
Function .onGUIEnd
	; Destroy must not have /NOUNLOAD so NSIS will be able to unload and delete BgImage before it exits
	BgImage::Destroy
FunctionEnd
 
!endif
 
; beginning (invisible) section
Section
 
  WriteRegStr HKLM "${regkey}" "Install_Dir" "$INSTDIR"
  ; write uninstall strings
  WriteRegStr HKLM "${uninstkey}" "DisplayName" "$(prodname)"
  WriteRegStr HKLM "${uninstkey}" "UninstallString" '"$INSTDIR\${uninstaller}"'
 
!ifdef filetype
  WriteRegStr HKCR "${filetype}" "" "${prodname}"
!endif
 
  WriteRegStr HKCR "${prodname}\Shell\open\command\" "" '"$INSTDIR\${exec}" "%1"'
 
!ifdef icon
  WriteRegStr HKCR "${prodname}\DefaultIcon" "" "$INSTDIR\${icon}"
!endif
 
  SetOutPath $INSTDIR
 
 
; package all files, recursively, preserving attributes
; assume files are in the correct places
File "${srcdir}\${exec}"

File "${srcdir}\LibPadlock.dll"
 
!ifdef licensefile
File "${srcdir}\${licensefile}"
!endif
 
!ifdef notefile
File "${srcdir}\${notefile}"
!endif
 
!ifdef icon
File "${srcdir}\${icon}"
!endif
 
; any application-specific files
!ifdef files
!include "${files}"
!endif
 
  WriteUninstaller "${uninstaller}"
  
SectionEnd
 
; create shortcuts
Section
  
  CreateDirectory "${startmenu}"
  SetOutPath $INSTDIR ; for working directory
  
  CreateShortCut "${startmenu}\$(prodname).lnk" "$INSTDIR\${exec}" "" "$INSTDIR\${icon}"
  CreateShortCut "${startmenu}\Uninstall $(prodname).lnk" "$INSTDIR\${uninstaller}" "" "$INSTDIR\${icon}"

  CreateShortCut "$QUICKLAUNCH\$(prodname).lnk" "$INSTDIR\${exec}" "" "$INSTDIR\${icon}"
  
!ifdef notefile
  CreateShortCut "${startmenu}\Release Notes.lnk" "$INSTDIR\${notefile}"
!endif
 
!ifdef helpfile
  CreateShortCut "${startmenu}\Documentation.lnk" "$INSTDIR\${helpfile}"
!endif
 
!ifdef website
  CreateShortCut "${startmenu}\Web Site.lnk" "${website}" "URL"
!endif
 
!ifdef notefile
ExecShell "open" "$INSTDIR\${notefile}"
!endif
 
SectionEnd
 
; Uninstaller
; All section names prefixed by "Un" will be in the uninstaller
 
;UninstallText "This will uninstall $(prodname)."
 
!ifdef icon
UninstallIcon "${icon}"
!endif
 
Section "Uninstall"
 
  DeleteRegKey HKLM "${uninstkey}"
  DeleteRegKey HKLM "${regkey}"
    
  RMDir /r "$INSTDIR\*.*"
  RMDir "$INSTDIR"
 
  Delete "${startmenu}\*.*"
  RMDir "${startmenu}"

  Delete "$QUICKLAUNCH\$(prodname).lnk"
  
!ifdef filetype
  DeleteRegKey HKCR "${filetype}"
!endif
 
DeleteRegKey HKCR "${prodname}" 
 
SectionEnd