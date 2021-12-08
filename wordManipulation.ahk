#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
; #Warn  ; Enable warnings to assist with detecting common errors.
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.


SetTitleMatchMode, RegEx
#IfWinActive ahk_class OpusApp
^n::
;WinGetClass, class, A
;MsgBox The active window's class is "%class%"
Send, {Down}
return

^p::
Send, {Up}
return

^e::
Send, {End}
return

^a::
Send, {Home}
return

^f::
Send, {Right}
return

!f::
Send, ^{Right}
return

^b::
Send, {Left}
return

!b::
Send, ^{Left}
return

^d::
Send, {Del}
return

!d::
Send, ^{Del}
return