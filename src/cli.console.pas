unit CLI.Console;

{$mode objfpc}{$H+}{$J-}

{ This unit provides console output functionality with color support.
  It handles both Windows and ANSI terminal color codes, cursor movement,
  and text formatting. The unit automatically detects the platform and
  uses appropriate methods for console manipulation. }

interface

type
  { Console colors enumeration
    Supports both standard and bright colors
    Note: Not all terminals support bright colors }
  TConsoleColor = (
    ccBlack, ccBlue, ccGreen, ccCyan, 
    ccRed, ccMagenta, ccYellow, ccWhite,
    ccBrightBlack, ccBrightBlue, ccBrightGreen, ccBrightCyan,
    ccBrightRed, ccBrightMagenta, ccBrightYellow, ccBrightWhite
  );

  { TConsole - Static class for console operations
    Provides:
    - Color text output
    - Cursor movement
    - Line clearing
    - Position saving/restoring }
  TConsole = class
  private
    class var FDefaultAttr: Word;  // Stores initial console attributes
    
    { Initializes console settings
      Called automatically in initialization section
      Platform-specific: Stores initial attributes on Windows }
    class procedure InitConsole;
  public
    { Sets text foreground color
      @param Color The color to set for subsequent text output }
    class procedure SetForegroundColor(const Color: TConsoleColor);
    
    { Sets text background color
      @param Color The color to set for text background }
    class procedure SetBackgroundColor(const Color: TConsoleColor);
    
    { Resets colors to console defaults
      Note: Uses stored default attributes on Windows }
    class procedure ResetColors;
    
    { Clears the current line
      Note: Moves cursor to start of line }
    class procedure ClearLine;
    
    { Moves cursor up specified number of lines
      @param Lines Number of lines to move up (default 1) }
    class procedure MoveCursorUp(const Lines: Integer = 1);
    
    { Moves cursor down specified number of lines
      @param Lines Number of lines to move down (default 1) }
    class procedure MoveCursorDown(const Lines: Integer = 1);
    
    { Moves cursor left specified number of columns
      @param Columns Number of columns to move left (default 1) }
    class procedure MoveCursorLeft(const Columns: Integer = 1);
    
    { Moves cursor right specified number of columns
      @param Columns Number of columns to move right (default 1) }
    class procedure MoveCursorRight(const Columns: Integer = 1);
    
    { Saves current cursor position
      Note: Can be restored with RestoreCursorPosition }
    class procedure SaveCursorPosition;
    
    { Restores previously saved cursor position
      Note: Must be preceded by SaveCursorPosition }
    class procedure RestoreCursorPosition;
    
    { Writes text without line ending
      @param Text The text to write }
    class procedure Write(const Text: string); overload;
    
    { Writes colored text without line ending
      @param Text The text to write
      @param FgColor The color to use for the text }
    class procedure Write(const Text: string; const FgColor: TConsoleColor); overload;
    
    { Writes text with line ending
      @param Text The text to write }
    class procedure WriteLn(const Text: string); overload;
    
    { Writes colored text with line ending
      @param Text The text to write
      @param FgColor The color to use for the text }
    class procedure WriteLn(const Text: string; const FgColor: TConsoleColor); overload;
  end;

implementation

uses
{$IFDEF WINDOWS}
  Windows;
{$ELSE}
  BaseUnix, termio;
{$ENDIF}

{ InitConsole: Platform-specific console initialization
  Windows: Stores initial console attributes
  Unix: No initialization needed (uses ANSI escape sequences) }
class procedure TConsole.InitConsole;
{$IFDEF WINDOWS}
var
  Handle: THandle;
  Info: TConsoleScreenBufferInfo;
begin
  Handle := GetStdHandle(STD_OUTPUT_HANDLE);
  if GetConsoleScreenBufferInfo(Handle, Info) then
    FDefaultAttr := Info.wAttributes
  else
    FDefaultAttr := $07;  // Default to light gray on black
{$ELSE}
begin
  // Nothing needed for ANSI terminals
  // They handle reset via ANSI escape sequence #27'[0m'
{$ENDIF}
end;

{ SetForegroundColor: Sets text color
  @param Color The color to use for text
  Platform-specific implementation:
  - Windows: Uses SetConsoleTextAttribute
  - Unix: Uses ANSI escape sequences }
class procedure TConsole.SetForegroundColor(const Color: TConsoleColor);
{$IFDEF WINDOWS}
var
  Handle: THandle;
  Info: TConsoleScreenBufferInfo;
begin
  Handle := GetStdHandle(STD_OUTPUT_HANDLE);
  GetConsoleScreenBufferInfo(Handle, Info);
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

{ SetBackgroundColor: Sets background color
  @param Color The color to use for background
  Platform-specific implementation:
  - Windows: Uses SetConsoleTextAttribute with color shifted
  - Unix: Uses ANSI escape sequences }
class procedure TConsole.SetBackgroundColor(const Color: TConsoleColor);
{$IFDEF WINDOWS}
var
  Handle: THandle;
  Info: TConsoleScreenBufferInfo;
begin
  Handle := GetStdHandle(STD_OUTPUT_HANDLE);
  GetConsoleScreenBufferInfo(Handle, Info);
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

{ ResetColors: Restores default console colors
  Platform-specific:
  - Windows: Restores saved attributes
  - Unix: Uses ANSI reset sequence }
class procedure TConsole.ResetColors;
{$IFDEF WINDOWS}
var
  Handle: THandle;
begin
  Handle := GetStdHandle(STD_OUTPUT_HANDLE);
  SetConsoleTextAttribute(Handle, FDefaultAttr);
{$ELSE}
begin
  System.Write(#27'[0m');
{$ENDIF}
end;

{ ClearLine: Clears current line and returns cursor to start
  Uses carriage return and spaces for universal compatibility }
class procedure TConsole.ClearLine;
begin
  System.Write(#13);  // Carriage return
  System.Write('                                                  ');  // Clear line
  System.Write(#13);  // Return to start
end;

{ MoveCursorUp: Moves cursor up specified lines
  @param Lines Number of lines to move up }
class procedure TConsole.MoveCursorUp(const Lines: Integer);
begin
  System.Write(#27'[', Lines, 'A');
end;

{ MoveCursorDown: Moves cursor down specified lines
  @param Lines Number of lines to move down }
class procedure TConsole.MoveCursorDown(const Lines: Integer);
begin
  System.Write(#27'[', Lines, 'B');
end;

{ MoveCursorLeft: Moves cursor left specified columns
  @param Columns Number of columns to move left }
class procedure TConsole.MoveCursorLeft(const Columns: Integer);
begin
  System.Write(#27'[', Columns, 'D');
end;

{ MoveCursorRight: Moves cursor right specified columns
  @param Columns Number of columns to move right }
class procedure TConsole.MoveCursorRight(const Columns: Integer);
begin
  System.Write(#27'[', Columns, 'C');
end;

{ SaveCursorPosition: Saves current cursor position
  Uses ANSI sequence that works on most terminals }
class procedure TConsole.SaveCursorPosition;
begin
  System.Write(#27'7');
end;

{ RestoreCursorPosition: Restores saved cursor position
  Uses ANSI sequence that works on most terminals }
class procedure TConsole.RestoreCursorPosition;
begin
  System.Write(#27'8');
end;

{ Write: Outputs text without line ending
  @param Text The text to write }
class procedure TConsole.Write(const Text: string);
begin
  System.Write(Text);
end;

{ Write: Outputs colored text without line ending
  @param Text The text to write
  @param FgColor The color to use for the text
  Note: Automatically resets colors after writing }
class procedure TConsole.Write(const Text: string; const FgColor: TConsoleColor);
begin
  SetForegroundColor(FgColor);
  System.Write(Text);
  ResetColors;
end;

{ WriteLn: Outputs text with line ending
  @param Text The text to write }
class procedure TConsole.WriteLn(const Text: string);
begin
  System.WriteLn(Text);
end;

{ WriteLn: Outputs colored text with line ending
  @param Text The text to write
  @param FgColor The color to use for the text
  Note: Automatically resets colors after writing }
class procedure TConsole.WriteLn(const Text: string; const FgColor: TConsoleColor);
begin
  SetForegroundColor(FgColor);
  System.WriteLn(Text);
  ResetColors;
end;

initialization
  TConsole.InitConsole;
end.