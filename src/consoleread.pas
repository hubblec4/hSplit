unit ConsoleRead;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils
 {$IfDef UNIX}
 , BaseUnix, termio
 {$Else}
 , Windows
 {$EndIf}
 ;

 function ReadSequence(Blocking: boolean = True): string;

implementation

{$IfDef UNIX}

function ReadChar(Blocking: boolean = True): char;
var
  oTIO, nTIO: Termios;
    {$IfDef NonBlockingStdIn}
  flags,
    {$Else}
  fdsin: tfdSet;
    {$EndIf}
  res: integer;
begin
  res := 1;
  Result := #0;
  TCGetAttr(1, oTIO);
  nTIO := oTIO;
  CFMakeRaw(nTIO);
  TCSetAttr(1, TCSANOW, nTIO);
  if not Blocking then
  begin
    {$ifDef NonBlockingStdIn}
    flags := FpFcntl(StdInputHandle, F_GetFl, 0);
    FpFcntl(StdInputHandle, F_SetFl, flags or O_NONBLOCK);
    {$Else}
    fpFD_ZERO(fdsin);
    fpFD_SET(StdInputHandle, fdsin);
    res := fpSelect(StdInputHandle + 1, @fdsin, nil, nil, 0);
    {$EndIf}
  end;
  if res > 0 then
    res := FpRead(StdInputHandle, Result, 1);

  {$ifDef NonBlockingStdIn}
  if res = 0 then
    Result := #0;
  {$EndIf}

  //restore settings
  TCSetAttr(1, TCSANOW, oTIO);
  {$ifDef NonBlockingStdIn}
  if not Blocking then
    FpFcntl(StdInputHandle, F_SetFl, flags);
  {$EndIf}
end;

{$Else}

var
  NextChars: string;

// found at http://www.cplusplus.com/forum/articles/19975/
function ReadChar(Blocking: boolean = True): char;
var
  STDInHandle: HANDLE;
  ConsoleInput: INPUT_RECORD;
  RecordCount: cardinal=0;
begin
  if NextChars.Length > 0 then
  begin
    Result := NextChars.Chars[0];
    NextChars := NextChars.Substring(1);
    Exit;
  end;
  STDInHandle := GetStdHandle(STD_INPUT_HANDLE);
  GetNumberOfConsoleInputEvents(STDInHandle, RecordCount);
  if Blocking or (RecordCount > 0) then
    while ReadConsoleInputA(STDInHandle, ConsoleInput, 1, RecordCount) do
      if (ConsoleInput.EventType = KEY_EVENT) and
        (ConsoleInput.Event.KeyEvent.wVirtualKeyCode <> VK_SHIFT) and
        (ConsoleInput.Event.KeyEvent.wVirtualKeyCode <> VK_MENU) and
        (ConsoleInput.Event.KeyEvent.wVirtualKeyCode <> VK_CONTROL) and
        (ConsoleInput.Event.KeyEvent.bKeyDown) then
      begin
        if (ConsoleInput.Event.KeyEvent.wVirtualKeyCode >= VK_PRIOR) and
          (ConsoleInput.Event.KeyEvent.wVirtualKeyCode <= VK_HOME) then
        begin
          case ConsoleInput.Event.KeyEvent.wVirtualKeyCode of
            VK_HOME: NextChars := '[H';
            VK_END: NextChars := '[F';
            VK_PRIOR: NextChars := '[5~';
            VK_NEXT: NextChars := '[6~';
          end;
          Result := #27;
        end
        else if (ConsoleInput.Event.KeyEvent.wVirtualKeyCode >= VK_F1) and
          (ConsoleInput.Event.KeyEvent.wVirtualKeyCode <= VK_F12) then
        begin
          case ConsoleInput.Event.KeyEvent.wVirtualKeyCode of   // F-Keys
            VK_F1: NextChars := #79'P';
            VK_F2: NextChars := #79'Q';
            VK_F3: NextChars := #79'R';
            VK_F4: NextChars := #79'S';
            VK_F5: NextChars := #91#49#53'~';
            VK_F6: NextChars := #91#49#55'~';
            VK_F7: NextChars := #91#49#56'~';
            VK_F8: NextChars := #91#49#57'~';
            VK_F9: NextChars := #91#50#48'~';
            VK_F10: NextChars := #91#50#49'~';
            VK_F11: NextChars := #91#50#50'~';
            VK_F12: NextChars := #91#50#52'~';
          end;
          Result := #27;
        end
        else if ConsoleInput.Event.KeyEvent.dwControlKeyState and ($8 or $4) <> 0 then
        begin
          if (ConsoleInput.Event.KeyEvent.wVirtualKeyCode >= Ord('A')) and
            (ConsoleInput.Event.KeyEvent.wVirtualKeyCode <= Ord('Z')) then
            Result := chr(ConsoleInput.Event.KeyEvent.wVirtualKeyCode - Ord('A') + 1)
          else if (ConsoleInput.Event.KeyEvent.wVirtualKeyCode >= Ord('3')) and
            (ConsoleInput.Event.KeyEvent.wVirtualKeyCode <= Ord('7')) then
            Result := chr(ConsoleInput.Event.KeyEvent.wVirtualKeyCode - Ord('3') + 27);
        end
        else
          case ConsoleInput.Event.KeyEvent.AsciiChar of
            #8, #27, #13, #32..#254:
              if ConsoleInput.Event.KeyEvent.dwControlKeyState and $2 = $2 then
              begin
                NextChars := ConsoleInput.Event.KeyEvent.AsciiChar;
                Result := #27;
              end
              else
                Result := ConsoleInput.Event.KeyEvent.AsciiChar;
            #9:
            begin
              if ConsoleInput.Event.KeyEvent.dwControlKeyState and $0010 = $0010 then
              begin
                NextChars := '[Z';
                Result := #27;
              end
              else
                Result := #9;
            end;
            #0:
            begin
              // Arrows
              SetLength(NextChars, 2);
              NextChars[1] := '[';
              case ConsoleInput.Event.KeyEvent.wVirtualKeyCode of
                $25: NextChars[2] := 'D';
                $26: NextChars[2] := 'A';
                $27: NextChars[2] := 'C';
                $28: NextChars[2] := 'B';
              end;
              Result := #27;
            end;
          end;
        Exit;
      end
      else
      if not Blocking then
        break;
  Result := #0;
end;

{$EndIf}

function ReadSequence(Blocking: boolean = True): string;
var
  c: char;
  l: integer;
begin
  SetLength(Result, 1);
  Result[1] := ReadChar(Blocking);
  if Result[1] = #0 then
  begin
    SetLength(Result, 0);
    Exit;
  end
  else if Result[1] <> #27 then
    exit;
  l := 1;
  repeat
    c := ReadChar(False);
    if c > #0 then
    begin
      Inc(l);
      if l > Result.Length then
        SetLength(Result, Result.Length * 2);
      Result[l] := c;
    end;
  until c = #0;
  SetLength(Result, l);
end;

end.

