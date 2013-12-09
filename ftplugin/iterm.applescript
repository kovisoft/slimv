#! /usr/bin/osascript
-- joinList from Geert Vanderkelen @ bit.ly/1gRPYbH
-- toDo push new terminal to background after creation
to joinList(aList, delimiter)
    set retVal to ""
    set prevDelimiter to AppleScript's text item delimiters
    set AppleScript's text item delimiters to delimiter
    set retVal to aList as string
    set AppleScript's text item delimiters to prevDelimiter
    return retVal
end joinList

on run arg
    set thecommand to joinList(arg, " ")
    tell application "iTerm"
        activate
        set myterm to (make new terminal)
        tell myterm
            set mysession to (launch session "Default")
            tell mysession
                write text thecommand
            end tell
        end tell
    end tell
end run
