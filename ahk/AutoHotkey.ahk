#SingleInstance, Force
#Include %A_ScriptDir%

; Make the current window stay on top.
#f::Winset, Alwaysontop, , A

; [Q]uit the current window.
#q::WinClose, A

#Include Gdip_All.ahk

; Make a screenshot between two points on the screen.
PrintScreen::
KeyWait, LButton, D
MouseGetPos, x1, y1
KeyWait, LButton

KeyWait, LButton, D
MouseGetPos, x2, y2
KeyWait, LButton

if (x1 > x2)
{
    startX := x2
    endX := x1
}
else
{
    startX := x1
    endX := x2
}

if (y1 > y2)
{
    startY := y2
    endY := y1
}
else
{
    startY := y1
    endY := y2
}

width := endX - startX
height := endY - startY

token := Gdip_Startup()

screenshot := Gdip_BitmapFromScreen(startX "|" startY "|" width "|" height)
Gdip_SetBitmapToClipboard(screenshot)
Gdip_DisposeImage(screenshot)

Gdip_Shutdown(token)
return
