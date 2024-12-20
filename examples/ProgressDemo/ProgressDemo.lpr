program ProgressDemo;

{$mode objfpc}{$H+}

{ This demo shows how to use progress indicators (spinner and progress bar)
  in a CLI application. It demonstrates:
  1. How to create and use spinners for indeterminate progress
  2. How to create and use progress bars for determinate progress
  3. Proper error handling with try-finally blocks
  4. Parameter handling with default values
  5. Colored console output }

uses
  SysUtils,      // For basic system utilities like Sleep and StrToIntDef
  CLI.Interfaces,// Core interfaces for the CLI framework
  CLI.Application,// Main application functionality
  CLI.Parameter, // Parameter handling
  CLI.Command,   // Base command implementation
  CLI.Console,   // Colored console output
  CLI.Progress;  // Progress indicators (spinner/progress bar)

type
  { TProcessCommand - A command that demonstrates both spinner and progress bar
    This shows how to handle long-running operations with visual feedback }
  TProcessCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

{ TProcessCommand implementation
  Returns: 0 for success, non-zero for failure
  Shows two phases of operation:
  1. Preparation phase with spinner (indeterminate progress)
  2. Processing phase with progress bar (determinate progress) }
function TProcessCommand.Execute: Integer;
var
  Count: Integer;     // Number of items to process
  CountStr: string;   // String value from parameter
  i: Integer;         // Loop counter
  Spinner: IProgressIndicator;    // For preparation phase
  ProgressBar: IProgressIndicator;// For processing phase
begin
  Result := 0;  // Initialize to success

  // Get number of items to process from parameter
  // If parameter is missing or invalid, default to 5
  if not GetParameterValue('--count', CountStr) then
    Count := 5
  else
    Count := StrToIntDef(CountStr, 5);  // Convert string to integer, default to 5 if invalid

  { First phase: Show spinner while "preparing"
    Spinners are good for operations where progress can't be measured }
  TConsole.WriteLn('Preparing to process files...', ccCyan);
  Spinner := CreateSpinner(ssLine);  // Create a line-style spinner
  Spinner.Start;  // Always start before using
  
  try
    // Simulate preparation work
    for i := 1 to Count do
    begin
      Spinner.Update(0);  // Update spinner (parameter ignored for spinners)
      Sleep(500);  // Simulate work (don't use in real code)
    end;
  finally
    // Always stop the spinner in finally block to ensure proper cleanup
    Spinner.Stop;
  end;

  { Second phase: Show progress bar while "processing"
    Progress bars are good when you know the total steps }
  TConsole.WriteLn('Processing files...', ccCyan);
  ProgressBar := CreateProgressBar(Count);  // Create progress bar with total count
  ProgressBar.Start;
  
  try
    // Simulate processing files
    for i := 1 to Count do
    begin
      // Update progress bar with current progress (i/Count)
      ProgressBar.Update(i);
      
      // Simulate work (don't use Sleep in real code)
      Sleep(500);
      
      // Show what we're doing (progress details)
      TConsole.WriteLn(Format(' Processed file %d of %d', [i, Count]), ccWhite);
    end;
  finally
    // Always stop the progress bar in finally block
    ProgressBar.Stop;
  end;

  // Show success message in green
  TConsole.WriteLn('All files processed successfully!', ccGreen);
end;

var
  App: ICLIApplication;  // Main application interface
  Cmd: TProcessCommand; // Our process command
begin
  try
    // Create main application with name and version
    App := CreateCLIApplication('ProgressDemo', '1.0.0');
    (App as TCLIApplication).DebugMode := False; // Disable debug output for cleaner display

    // Create and configure process command
    Cmd := TProcessCommand.Create('process', 'Process files with progress indication');
    // Add optional count parameter
    Cmd.AddParameter(CreateParameter(
      '-c',           // Short flag
      '--count',      // Long flag
      'Number of files to process', // Description
      False,          // Not required
      ptInteger,      // Parameter type
      '5'            // Default value
    ));

    // Register command with application
    App.RegisterCommand(Cmd);
    Cmd := nil;  // Clear reference as App now owns the command

    // Execute the application and get exit code
    ExitCode := App.Execute;
  except
    // Handle any unexpected errors
    on E: Exception do
    begin
      TConsole.WriteLn('Error: ' + E.Message, ccRed);
      ExitCode := 1;  // Return error code
    end;
  end;
end. 
