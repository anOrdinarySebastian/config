#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
; #Warn  ; Enable warnings to assist with detecting common errors.
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.


SetTitleMatchMode, RegEx
#IfWinActive ahk_class MozillaWindowClass
^n::
; WinGetClass, class, A
; MsgBox The active window's class is "%class%"
Send, {Down down}{Down up}
return

^w::
return