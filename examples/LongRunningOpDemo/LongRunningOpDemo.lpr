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
    Progress: IProgressIndicator;
    Spinner: IProgressIndicator;
    i: integer;
    VerboseStr, CountStr, LogLevelStr, TagsStr, StartAfterStr, ApiKeyStr: string;
    InputDir, OutputDir: string;
    FileCount: Integer;
    Verbose: boolean;
    Tags: TStringList;
    StartAfterDate: TDateTime;
  begin
    Files := TStringList.Create;
    Tags := TStringList.Create;
    try
      // Get basic parameters
      Verbose := False;
      if GetParameterValue('--verbose', VerboseStr) then
        Verbose := StrToBoolDef(VerboseStr, False);

      FileCount := 5;  // Default value
      if GetParameterValue('--count', CountStr) then
        FileCount := StrToIntDef(CountStr, 5);

      // Get path parameters
      if not GetParameterValue('--input', InputDir) then
      begin
        TConsole.WriteLn('Error: Input directory is required', ccRed);
        Exit(1);
      end;
      
      if not GetParameterValue('--output', OutputDir) then
        OutputDir := InputDir;  // Default to input directory

      // Get advanced parameters
      GetParameterValue('--log-level', LogLevelStr);  // Will use default from parameter
      
      if GetParameterValue('--tags', TagsStr) then
      begin
        Tags.Delimiter := ',';
        Tags.DelimitedText := TagsStr;
      end;

      if GetParameterValue('--start-after', StartAfterStr) then
      begin
        try
          StartAfterDate := StrToDateTime(StartAfterStr);
        except
          on E: Exception do
          begin
            TConsole.WriteLn('Error: Invalid date/time format. Use YYYY-MM-DD HH:MM:SS', ccRed);
            Exit(1);
          end;
        end;
      end;

      // API key is sensitive - don't log it
      GetParameterValue('--api-key', ApiKeyStr);

      // Show configuration
      TConsole.WriteLn('Configuration:', ccCyan);
      TConsole.WriteLn(Format('  Input Directory: %s', [InputDir]), ccCyan);
      TConsole.WriteLn(Format('  Output Directory: %s', [OutputDir]), ccCyan);
      TConsole.WriteLn(Format('  Log Level: %s', [LogLevelStr]), ccCyan);
      if Tags.Count > 0 then
        TConsole.WriteLn(Format('  Tags: %s', [Tags.DelimitedText]), ccCyan);
      if StartAfterStr <> '' then
        TConsole.WriteLn(Format('  Processing files after: %s', [StartAfterStr]), ccCyan);
      if ApiKeyStr <> '' then
        TConsole.WriteLn('  API Key: ***', ccCyan);

      // Simulate finding files
      TConsole.WriteLn(Format('Finding files in %s...', [InputDir]), ccCyan);
      Spinner := CreateSpinner(TSpinnerStyle.ssDots);
      Spinner.Start;
      try
        // Add files with spinner animation
        for i := 1 to FileCount do
        begin
          Spinner.Update(0);
          Sleep(300);  // Simulate searching
          Files.Add(Format('%s\file%d.txt', [InputDir, i]));
        end;
      finally
        Spinner.Stop;
      end;

      // Process files with a progress bar
      Progress := CreateProgressBar(Files.Count, 20);  // 20 chars wide
      Progress.Start;
      try
        for i := 0 to Files.Count - 1 do
        begin
          if Verbose then
            TConsole.WriteLn(Format(' Processing: %s', [Files[i]]), ccCyan);

          ProcessFile(Files[i]);
          Progress.Update(i + 1);
          Sleep(500); // Simulate work
        end;

        TConsole.WriteLn(' All files processed successfully!', ccGreen);
        Result := 0;
      finally
        Progress.Stop;
      end;
    finally
      Files.Free;
      Tags.Free;
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
  
  // Basic parameters
  Cmd.AddFlag('-v', '--verbose', 'Show detailed progress');
  Cmd.AddIntegerParameter('-c', '--count', 'Number of files to process', False, '5');
  
  // File and path handling
  Cmd.AddPathParameter('-i', '--input', 'Input directory to process', True);
  Cmd.AddPathParameter('-o', '--output', 'Output directory for results');
  
  // Advanced parameter types
  Cmd.AddEnumParameter('-l', '--log-level', 'Logging verbosity level', 'debug|info|warn|error', False, 'info');
  Cmd.AddArrayParameter('-t', '--tags', 'Tags to apply to processed files');
  Cmd.AddDateTimeParameter('-s', '--start-after', 'Only process files modified after this date/time');
  Cmd.AddPasswordParameter('-k', '--api-key', 'API key for external service');

  // Register command and run the application
  App.RegisterCommand(Cmd);
  ExitCode := App.Execute;
end.
