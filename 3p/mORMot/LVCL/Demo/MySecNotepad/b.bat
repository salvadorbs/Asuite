@echo off
upx2 -9 MySecNotepad.exe
copy MySecNotepad.exe E:\backup
"c:\program files\nsis\makensisw.exe" MySecNotepad.nsi
