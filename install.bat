@echo off

rem My first attempt at batch scripting.
rem
rem The script acts as a GNU/Stow alternative for Windows, magically creates
rem and autostarts an MPD service.

rem Args:
rem %1 - if present, download the required software.

rem Link Emacs user directory.

set EmacsDir="%AppData%\.emacs.d"

if not exist %EmacsDir% (
    mklink /d %EmacsDir% emacs\.emacs.d
)

rem * MPD

set MpdDir="C:\Program Files\mpd"
set MpdExeURL="https://www.musicpd.org/download/win32/0.22.3/mpd.exe"

if not exist %MpdDir% (
    mkdir %MpdDir%
)

rem Download the executable.
if [%1] neq [] (
   bitsadmin /transfer "Download MPD" %MpdExeURL% %MpdDir%\mpd.exe
)

rem Link the configuration file.
mklink %MpdDir%\mpd.conf mpd/mpd.conf


rem Create and start the MPD service. Very hacky.
sc create mpd start= auto binPath= "\"C:\Program Files\mpd\mpd.exe\" \"C:\Program Files\mpd\mpd.conf\""
sc start mpd

rem * AutoHotKey

rem Create the startup task.
set AhkConfig=%UserProfile%\dotfiles\ahk\AutoHotkey.ahk
schtasks /Create /F /SC ONSTART /TN AHK /TR "\"C:\Program Files\AutoHotkey\AutoHotkey.exe\" %AhkConfig%"

exit /b 0
