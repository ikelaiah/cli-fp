{
  Long Running Operation Demo

  This example demonstrates how to create a command-line application that shows progress
  indicators for long-running operations. It showcases several key features of the CLI framework:

  1. Progress indicators (spinner and progress bar)
  2. Command parameters (verbose flag)
  3. Colored console output
  4. Basic command structure

  How to run:
  Option 1 - Basic usage with progress bar only:
  ```
  $ LongRunningOpDemo.exe process
  Finding files...

  [====================] 100% All files processed successfully!
  ```

  Option 2 - Verbose mode shows detailed progress:
  ```
  $ LongRunningOpDemo.exe process -v true
  Finding files...

  Processing: file1.txt
  [=======             ]  33% Processing: file2.txt
  [=============       ]  67% Processing: file3.txt
  [====================] 100% All files processed successfully!
  ```
}
program LongRunningOpDemo;

{ Compiler directives:
  - $mode objfpc: Use Object Pascal mode for modern OOP features
  - $H+: Use long strings (AnsiString) instead of short strings
  - $J-: Disable writeable typed constants for safety }
{$mode objfpc}{$H+}{$J-}

{ Import required units from the CLI framework }
uses
  SysUtils,
  Classes,
  CLI.Interfaces,    // Core interfaces
  CLI.Application,   // Application creation
  CLI.Command,       // Command base class
  CLI.Parameter,     // Parameter handling
  CLI.Progress,      // Progress indicators
  CLI.Console;       // Colored output

type
  { Define a command that processes files with progress indication }
  TProcessCommand = class(TBaseCommand)
  private
    { Simulates processing a single file }
    procedure ProcessFile(const FileName: string);
  public
    { Main execution method for the command }
    function Execute: integer; override;
  end;

  function TProcessCommand.Execute: integer;
  var
    Files: TStringList;
    Progress: IProgressIndicator;    // Progress bar interface
    Spinner: IProgressIndicator;     // Spinner interface for file search
    i, FileCount: integer;
    VerboseStr: string;             // Raw parameter value
    Verbose: boolean;               // Parsed verbose flag
  begin
    Files := TStringList.Create;
    try
      // Simulate finding files
      FileCount := 5;  // We know we'll add 5 files
      TConsole.WriteLn('Finding files...', ccCyan);
      Spinner := CreateSpinner(TSpinnerStyle.ssDots);
      Spinner.Start;
      try
        // Add files with spinner animation
        for i := 1 to FileCount do
        begin
          Spinner.Update(0);
          Sleep(300);  // Simulate searching
          Files.Add(Format('file%d.txt', [i]));
        end;
      finally
        Spinner.Stop;
      end;
      // Step 2: Check if verbose output is requested
      Verbose := False; // Default value
      if GetParameterValue('--verbose', VerboseStr) then
        Verbose := StrToBoolDef(VerboseStr, False);

      // Step 3: Process files with a progress bar
      Progress := CreateProgressBar(Files.Count, 20);  // 20 chars wide
      Progress.Start;
      try
        for i := 0 to Files.Count - 1 do
        begin
          if Verbose then
            TConsole.WriteLn(' Processing: ' + Files[i], ccCyan);

          ProcessFile(Files[i]);
          Progress.Update(i + 1);    // Update progress (1-based)
          Sleep(500); // Simulate work
        end;

        TConsole.WriteLn(' All files processed successfully!', ccGreen);
        Result := 0;
      finally
        Progress.Stop;
      end;
    finally
      Files.Free;
    end;
  end;

  procedure TProcessCommand.ProcessFile(const FileName: string);
  begin
    // Simulate file processing
    Sleep(100);
  end;

  { Main program setup }
var
  App: ICLIApplication;
  Cmd: TProcessCommand;
begin
  // Create the application with name and version
  App := CreateCLIApplication('MyApp', '1.0.0');

  // Create and configure the process command
  Cmd := TProcessCommand.Create('process', 'Process files');
  Cmd.AddParameter(CreateParameter('-v',           // Short flag
    '--verbose',    // Long flag
    'Show detailed progress',  // Description
    False,          // Not required
    ptBoolean,      // Boolean parameter type
    'false'         // Default value
    ));

  // Register command and run the application
  App.RegisterCommand(Cmd);
  ExitCode := App.Execute;
end.
