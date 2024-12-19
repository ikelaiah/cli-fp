unit CLI.Progress;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CLI.Interfaces, CLI.Console;

type
  { Spinner styles }
  TSpinnerStyle = (
    ssDots,    // ⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏
    ssLine,    // -\|/
    ssCircle   // ◐◓◑◒
  );

  { Base progress indicator class }
  TProgressIndicator = class(TInterfacedObject, IProgressIndicator)
  protected
    FActive: Boolean;
    procedure ClearLine; virtual;
  public
    procedure Start; virtual;
    procedure Stop; virtual;
    procedure Update(const Progress: Integer); virtual; abstract;
  end;

  { Spinner progress indicator }
  TSpinner = class(TProgressIndicator)
  private
    FStyle: TSpinnerStyle;
    FFrame: Integer;
    FFrames: array of string;
  public
    constructor Create(const AStyle: TSpinnerStyle);
    procedure Update(const Progress: Integer); override;
  end;

  { Progress bar indicator }
  TProgressBar = class(TProgressIndicator)
  private
    FTotal: Integer;
    FWidth: Integer;
    FLastProgress: Integer;
  public
    constructor Create(const ATotal: Integer; const AWidth: Integer = 10);
    procedure Update(const Progress: Integer); override;
  end;

{ Helper functions to create progress indicators }
function CreateSpinner(const Style: TSpinnerStyle = ssLine): IProgressIndicator;
function CreateProgressBar(const Total: Integer; const Width: Integer = 10): IProgressIndicator;

implementation

{ TProgressIndicator }
procedure TProgressIndicator.ClearLine;
var
  i:Integer;
begin
  Write(#13);
  for i := 1 to 80 do
    Write(' ');
  Write(#13);
end;

procedure TProgressIndicator.Start;
begin
  FActive := True;
end;

procedure TProgressIndicator.Stop;
begin
  FActive := False;
  WriteLn;
end;

{ TSpinner }
constructor TSpinner.Create(const AStyle: TSpinnerStyle);
begin
  inherited Create;
  FStyle := AStyle;
  FFrame := 0;
  
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
  
  ClearLine;
  Write(FFrames[FFrame]);
  FFrame := (FFrame + 1) mod Length(FFrames);
end;

{ TProgressBar }
constructor TProgressBar.Create(const ATotal: Integer; const AWidth: Integer = 10);
begin
  inherited Create;
  FTotal := ATotal;
  FWidth := AWidth;
  FLastProgress := -1;
end;

procedure TProgressBar.Update(const Progress: Integer);
var
  Percentage: Integer;
  FilledWidth: Integer;
  i: Integer;
begin
  if not FActive then Exit;
  
  // Calculate percentage based on current progress and total
  Percentage := Round((Progress / FTotal) * 100);
  
  // Only update if progress has changed
  if Percentage = FLastProgress then
    Exit;
    
  FLastProgress := Percentage;
  
  // Calculate how many blocks to fill
  FilledWidth := Round((Percentage / 100) * FWidth);
  
  ClearLine;
  Write('[');
  
  // Draw filled portion
  for i := 1 to FilledWidth do
    Write('=');
    
  // Draw empty portion
  for i := FilledWidth + 1 to FWidth do
    Write(' ');
    
  Write('] ', Percentage, '%');
end;

function CreateSpinner(const Style: TSpinnerStyle): IProgressIndicator;
begin
  Result := TSpinner.Create(Style);
end;

function CreateProgressBar(const Total: Integer; const Width: Integer): IProgressIndicator;
begin
  Result := TProgressBar.Create(Total, Width);
end;

end.
