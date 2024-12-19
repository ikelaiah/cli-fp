program ProgressDemo;

{$mode objfpc}{$H+}

uses
  SysUtils,
  CLI.Interfaces,
  CLI.Application,
  CLI.Parameter,
  CLI.Command,
  CLI.Console,
  CLI.Progress;

type
  { Process command that shows both spinner and progress bar }
  TProcessCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

{ TProcessCommand }
function TProcessCommand.Execute: Integer;
var
  Count: Integer;
  CountStr: string;
  i: Integer;
  Spinner: IProgressIndicator;
  ProgressBar: IProgressIndicator;
begin
  Result := 0;

  // Get number of items to process
  if not GetParameterValue('--count', CountStr) then
    Count := 5
  else
    Count := StrToIntDef(CountStr, 5);

  // First phase: Show spinner while "preparing"
  TConsole.WriteLn('Preparing to process files...', ccCyan);
  Spinner := CreateSpinner(ssDots);
  Spinner.Start;
  
  try
    // Simulate some preparation work
    for i := 1 to Count do
    begin
      Spinner.Update(0);
      Sleep(500);
    end;
  finally
    Spinner.Stop;
  end;

  // Second phase: Show progress bar while "processing"
  TConsole.WriteLn('Processing files...', ccCyan);
  ProgressBar := CreateProgressBar(Count);
  ProgressBar.Start;
  
  try
    // Simulate processing files
    for i := 1 to Count do
    begin
      // Update progress bar with current progress
      ProgressBar.Update(i);
      
      // Simulate work
      Sleep(500);
      
      // Show what we're doing
      TConsole.WriteLn(Format(' Processed file %d of %d', [i, Count]), ccWhite);
    end;
  finally
    ProgressBar.Stop;
  end;

  TConsole.WriteLn('All files processed successfully!', ccGreen);
end;

var
  App: ICLIApplication;
  Cmd: TProcessCommand;
begin
  try
    // Create main application
    App := CreateCLIApplication('ProgressDemo', '1.0.0');

    // Create and configure process command
    Cmd := TProcessCommand.Create('process', 'Process files with progress indication');
    Cmd.AddParameter(CreateParameter('-c', '--count', 'Number of files to process', False, ptInteger, '5'));

    // Register command
    App.RegisterCommand(Cmd);
    Cmd := nil;

    // Execute the application
    ExitCode := App.Execute;
  except
    on E: Exception do
    begin
      TConsole.WriteLn('Error: ' + E.Message, ccRed);
      ExitCode := 1;
    end;
  end;
end. 
