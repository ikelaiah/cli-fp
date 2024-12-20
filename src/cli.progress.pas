unit CLI.Progress;

{$mode objfpc}{$H+}

{ This unit provides progress indicator functionality for CLI applications.
  It includes two types of progress indicators:
  - Spinner: Shows an animated spinner for indeterminate progress
  - Progress Bar: Shows a progress bar for determinate progress
  
  The unit is designed to be flexible and easy to use, with support for
  different spinner styles and customizable progress bar widths. }

interface

uses
  Classes, SysUtils, CLI.Interfaces, CLI.Console;

type
  { TSpinnerStyle - Enumeration defining different spinner animation styles
    Available styles:
    - ssDots: Braille dots animation (⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏)
    - ssLine: Simple ASCII line animation (-\|/)
    - ssCircle: Unicode circle animation (◐◓◑◒)
    - ssSquare: Square rotation animation (◰◳◲◱)
    - ssArrow: Arrow rotation animation (←↖↑↗→↘↓↙)
    - ssBounce: Bouncing dot animation (⠁⠂⠄⠂)
    - ssBar: Vertical bar animation (▁▂▃▄▅▆▇█▇▆▅▄▃▂▁) }
  TSpinnerStyle = (
    ssDots,    // ⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏
    ssLine,    // -\|/
    ssCircle,  // ◐◓◑◒
    ssSquare,  // ◰◳◲◱
    ssArrow,   // ←↖↑↗→↘↓↙
    ssBounce,  // ⠁⠂⠄⠂
    ssBar      // ▁▂▃▄▅▆▇█▇▆▅▄▃▂▁
  );

  { TProgressIndicator - Base class for progress indicators
    Implements the IProgressIndicator interface and provides common
    functionality for both spinner and progress bar indicators.
    
    Usage:
    - Call Start to begin the progress indication
    - Call Update to update the display
    - Call Stop to end the progress indication }
  TProgressIndicator = class(TInterfacedObject, IProgressIndicator)
  protected
    FActive: Boolean;  // Tracks whether the indicator is currently active
    
    { Clears the current line in the console
      Used internally to prepare for updating the progress display }
    procedure ClearLine; virtual;
  public
    { Starts the progress indication
      After calling Start, the indicator will be displayed when Update is called }
    procedure Start; virtual;
    
    { Stops the progress indication and moves to the next line
      Should be called when the operation is complete }
    procedure Stop; virtual;
    
    { Updates the progress display
      @param Progress For spinner, ignored. For progress bar, current progress value }
    procedure Update(const Progress: Integer); virtual; abstract;
  end;

  { TSpinner - Animated spinner progress indicator
    Shows an animated spinner that rotates through a sequence of characters
    to indicate that an operation is in progress.
    
    Usage:
    - Create with desired style
    - Call Start to begin animation
    - Call Update regularly to animate
    - Call Stop when complete }
  TSpinner = class(TProgressIndicator)
  private
    FStyle: TSpinnerStyle;     // Current spinner style
    FFrame: Integer;           // Current frame in the animation
    FFrames: array of string;  // Animation frames for current style
  public
    { Creates a new spinner with specified style
      @param AStyle The visual style to use for the spinner }
    constructor Create(const AStyle: TSpinnerStyle);
    
    { Updates the spinner animation
      @param Progress Ignored for spinner (used for interface compatibility) }
    procedure Update(const Progress: Integer); override;
  end;

  { TProgressBar - Visual progress bar indicator
    Shows a progress bar that fills based on the current progress
    relative to a total value.
    
    Usage:
    - Create with total value and optional width
    - Call Start to begin display
    - Call Update with current progress
    - Call Stop when complete }
  TProgressBar = class(TProgressIndicator)
  private
    FTotal: Integer;        // Total value for 100% progress
    FWidth: Integer;        // Width of the progress bar in characters
    FLastProgress: Integer; // Last displayed progress to avoid unnecessary updates
  public
    { Creates a new progress bar
      @param ATotal Total value representing 100% progress
      @param AWidth Width of the progress bar in characters (default 10) }
    constructor Create(const ATotal: Integer; const AWidth: Integer = 10);
    
    { Updates the progress bar display
      @param Progress Current progress value (0 to FTotal)
      Note: Updates only when progress percentage changes }
    procedure Update(const Progress: Integer); override;
  end;

{ Helper functions to create progress indicators }

{ Creates a new spinner progress indicator
  @param Style The visual style for the spinner (default ssLine)
  @return IProgressIndicator interface for the created spinner }
function CreateSpinner(const Style: TSpinnerStyle = ssLine): IProgressIndicator;

{ Creates a new progress bar indicator
  @param Total The total value representing 100% progress
  @param Width The width of the progress bar in characters (default 10)
  @return IProgressIndicator interface for the created progress bar }
function CreateProgressBar(const Total: Integer; const Width: Integer = 10): IProgressIndicator;

implementation

{ TProgressIndicator }

procedure TProgressIndicator.ClearLine;
var
  i: Integer;
begin
  Write(#13);  // Carriage return
  for i := 1 to 80 do  // Clear entire line
    Write(' ');
  Write(#13);  // Return to start
end;

procedure TProgressIndicator.Start;
begin
  FActive := True;
end;

procedure TProgressIndicator.Stop;
begin
  FActive := False;
  WriteLn;  // Move to next line after stopping
end;

{ TSpinner }

constructor TSpinner.Create(const AStyle: TSpinnerStyle);
begin
  inherited Create;
  FStyle := AStyle;
  FFrame := 0;
  
  // Initialize animation frames based on style
  case FStyle of
    ssDots:
      FFrames := ['⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏'];
    ssLine:
      FFrames := ['-', '\', '|', '/'];
    ssCircle:
      FFrames := ['◐', '◓', '◑', '◒'];
  end;
end;

procedure TSpinner.Update(const Progress: Integer);
begin
  if not FActive then Exit;
  
  Write(#13);  // Return to start of line
  Write(FFrames[FFrame]);  // Write current frame
  FFrame := (FFrame + 1) mod Length(FFrames);  // Move to next frame
  Flush(Output);  // Ensure immediate display
  Sleep(100);  // Delay for animation
end;

{ TProgressBar }

constructor TProgressBar.Create(const ATotal: Integer; const AWidth: Integer = 10);
begin
  inherited Create;
  FTotal := ATotal;
  FWidth := AWidth;
  FLastProgress := -1;  // Initialize to invalid progress to force first update
end;

procedure TProgressBar.Update(const Progress: Integer);
var
  Percentage: Integer;
  FilledWidth: Integer;
  i: Integer;
  ProgressText: string;
begin
  if not FActive then Exit;
  
  // Calculate current percentage
  Percentage := Round((Progress / FTotal) * 100);
  
  // Skip update if percentage hasn't changed
  if Percentage = FLastProgress then
    Exit;
    
  FLastProgress := Percentage;
  
  // Calculate filled portion of bar
  FilledWidth := Round((Percentage / 100) * FWidth);
  
  // Create progress bar text: [====    ] XX%
  ProgressText := Format('[%-*.*s] %3d%%', [
    FWidth,           // Total width
    FWidth,           // Width for string formatting
    StringOfChar('=', FilledWidth) + StringOfChar(' ', FWidth - FilledWidth),
    Percentage
  ]);
  
  ClearLine;
  Write(ProgressText);
end;

{ Helper functions }

function CreateSpinner(const Style: TSpinnerStyle): IProgressIndicator;
begin
  Result := TSpinner.Create(Style);
end;

function CreateProgressBar(const Total: Integer; const Width: Integer): IProgressIndicator;
begin
  Result := TProgressBar.Create(Total, Width);
end;

end.
