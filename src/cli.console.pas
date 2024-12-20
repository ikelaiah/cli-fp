unit CLI.Console;

{$mode objfpc}{$H+}

interface

type
  TConsoleColor = (
    ccBlack, ccBlue, ccGreen, ccCyan, 
    ccRed, ccMagenta, ccYellow, ccWhite,
    ccBrightBlack, ccBrightBlue, ccBrightGreen, ccBrightCyan,
    ccBrightRed, ccBrightMagenta, ccBrightYellow, ccBrightWhite
  );

  { Console utilities }
  TConsole = class
  private
    class var FOldTextAttr: Word;
  public
    class procedure SetForegroundColor(const Color: TConsoleColor);
    class procedure SetBackgroundColor(const Color: TConsoleColor);
    class procedure ResetColors;
    class procedure ClearLine;
    class procedure MoveCursorUp(const Lines: Integer = 1);
    class procedure MoveCursorDown(const Lines: Integer = 1);
    class procedure MoveCursorLeft(const Columns: Integer = 1);
    class procedure MoveCursorRight(const Columns: Integer = 1);
    class procedure SaveCursorPosition;
    class procedure RestoreCursorPosition;
    class procedure Write(const Text: string); overload;
    class procedure Write(const Text: string; const FgColor: TConsoleColor); overload;
    class procedure WriteLn(const Text: string); overload;
    class procedure WriteLn(const Text: string; const FgColor: TConsoleColor); overload;
  end;

implementation

uses
{$IFDEF WINDOWS}
  Windows;
{$ELSE}
  BaseUnix, termio;
{$ENDIF}

class procedure TConsole.SetForegroundColor(const Color: TConsoleColor);
{$IFDEF WINDOWS}
var
  Handle: THandle;
  Info: TConsoleScreenBufferInfo;
begin
  Handle := GetStdHandle(STD_OUTPUT_HANDLE);
  GetConsoleScreenBufferInfo(Handle, Info);
  FOldTextAttr := Info.wAttributes;
  SetConsoleTextAttribute(Handle, (Info.wAttributes and $F0) or Ord(Color));
{$ELSE}
begin
  case Color of
    ccBlack: System.Write(#27'[30m');
    ccBlue: System.Write(#27'[34m');
    ccGreen: System.Write(#27'[32m');
    ccCyan: System.Write(#27'[36m');
    ccRed: System.Write(#27'[31m');
    ccMagenta: System.Write(#27'[35m');
    ccYellow: System.Write(#27'[33m');
    ccWhite: System.Write(#27'[37m');
    ccBrightBlack: System.Write(#27'[90m');
    ccBrightBlue: System.Write(#27'[94m');
    ccBrightGreen: System.Write(#27'[92m');
    ccBrightCyan: System.Write(#27'[96m');
    ccBrightRed: System.Write(#27'[91m');
    ccBrightMagenta: System.Write(#27'[95m');
    ccBrightYellow: System.Write(#27'[93m');
    ccBrightWhite: System.Write(#27'[97m');
  end;
{$ENDIF}
end;

class procedure TConsole.SetBackgroundColor(const Color: TConsoleColor);
{$IFDEF WINDOWS}
var
  Handle: THandle;
  Info: TConsoleScreenBufferInfo;
begin
  Handle := GetStdHandle(STD_OUTPUT_HANDLE);
  GetConsoleScreenBufferInfo(Handle, Info);
  FOldTextAttr := Info.wAttributes;
  SetConsoleTextAttribute(Handle, (Info.wAttributes and $0F) or (Ord(Color) shl 4));
{$ELSE}
begin
  case Color of
    ccBlack: System.Write(#27'[40m');
    ccBlue: System.Write(#27'[44m');
    ccGreen: System.Write(#27'[42m');
    ccCyan: System.Write(#27'[46m');
    ccRed: System.Write(#27'[41m');
    ccMagenta: System.Write(#27'[45m');
    ccYellow: System.Write(#27'[43m');
    ccWhite: System.Write(#27'[47m');
    ccBrightBlack: System.Write(#27'[100m');
    ccBrightBlue: System.Write(#27'[104m');
    ccBrightGreen: System.Write(#27'[102m');
    ccBrightCyan: System.Write(#27'[106m');
    ccBrightRed: System.Write(#27'[101m');
    ccBrightMagenta: System.Write(#27'[105m');
    ccBrightYellow: System.Write(#27'[103m');
    ccBrightWhite: System.Write(#27'[107m');
  end;
{$ENDIF}
end;

class procedure TConsole.ResetColors;
{$IFDEF WINDOWS}
var
  Handle: THandle;
  DefaultAttr: Word;
begin
  Handle := GetStdHandle(STD_OUTPUT_HANDLE);
  // On Windows, the default is usually white text on black background (0x07)
  DefaultAttr := $07;
  SetConsoleTextAttribute(Handle, DefaultAttr);
{$ELSE}
begin
  // ANSI reset sequence resets both foreground and background
  System.Write(#27'[0m');
{$ENDIF}
end;

class procedure TConsole.ClearLine;
begin
  System.Write(#13);  // Carriage return
  System.Write('                                                  ');  // Clear line
  System.Write(#13);  // Return to start
end;

class procedure TConsole.MoveCursorUp(const Lines: Integer);
begin
  System.Write(#27'[', Lines, 'A');
end;

class procedure TConsole.MoveCursorDown(const Lines: Integer);
begin
  System.Write(#27'[', Lines, 'B');
end;

class procedure TConsole.MoveCursorLeft(const Columns: Integer);
begin
  System.Write(#27'[', Columns, 'D');
end;

class procedure TConsole.MoveCursorRight(const Columns: Integer);
begin
  System.Write(#27'[', Columns, 'C');
end;

class procedure TConsole.SaveCursorPosition;
begin
  System.Write(#27'7');
end;

class procedure TConsole.RestoreCursorPosition;
begin
  System.Write(#27'8');
end;

class procedure TConsole.Write(const Text: string);
begin
  System.Write(Text);
end;

class procedure TConsole.Write(const Text: string; const FgColor: TConsoleColor);
begin
  SetForegroundColor(FgColor);
  System.Write(Text);
  ResetColors;
end;

class procedure TConsole.WriteLn(const Text: string);
begin
  System.WriteLn(Text);
end;

class procedure TConsole.WriteLn(const Text: string; const FgColor: TConsoleColor);
begin
  SetForegroundColor(FgColor);
  System.WriteLn(Text);
  ResetColors;
end;

end.