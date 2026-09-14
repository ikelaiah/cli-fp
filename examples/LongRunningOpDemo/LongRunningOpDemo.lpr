{
  Long Running Operation Demo

  This example demonstrates how to create a command-line application that shows progress
  indicators for long-running operations. It showcases several key features of the CLI framework:

  1. Progress indicators (spinner and progress bar)
  2. Typed command parameters and framework defaults
  3. Colored console output
  4. Basic command structure

  How to run:
  Option 1 - Basic usage with progress bar only:
  ```
  $ LongRunningOpDemo.exe process --input .
  ```
  The --input path is required; this demonstration uses the current directory.
  Add --output <directory> to see the simulated destination paths in verbose
  output.

  Option 2 - Verbose mode shows detailed progress:
  ```
  $ LongRunningOpDemo.exe process --input . --verbose
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
  CLI.Progress,      // Progress indicators
  CLI.Console;       // Colored output

type
  { Define a command that processes files with progress indication }
  TProcessCommand = class(TBaseCommand)
  private
    { Simulates processing a source file into the selected output directory. }
    procedure ProcessFile(const FileName, OutputDir: string;
      const Verbose: Boolean);
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
    VerboseStr, CountStr, LogLevelStr, TagsStr, ApiKeyStr: string;
    InputDir, OutputDir: string;
    FileCount: Integer;
    Verbose: boolean;
    Tags: TStringList;
  begin
    Files := TStringList.Create;
    Tags := TStringList.Create;
    try
      // The framework supplies these declared defaults when the options are absent.
      GetParameterValue('--verbose', VerboseStr);
      Verbose := SameText(VerboseStr, 'true');
      GetParameterValue('--count', CountStr);
      FileCount := StrToInt(CountStr);

      // Get path parameters
      if not GetParameterValue('--input', InputDir) then
      begin
        TConsole.WriteLn('Error: Input directory is required', ccRed);
        Exit(1);
      end;
      
      if not GetParameterValue('--output', OutputDir) then
        OutputDir := InputDir;  // Default to input directory

      // --log-level supplies its declared "info" default when it is absent.
      GetParameterValue('--log-level', LogLevelStr);
      
      if GetParameterValue('--tags', TagsStr) then
      begin
        Tags.Delimiter := ',';
        Tags.StrictDelimiter := True;
        Tags.DelimitedText := TagsStr;
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
          Files.Add(IncludeTrailingPathDelimiter(InputDir) +
            Format('file%d.txt', [i]));
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
          ProcessFile(Files[i], OutputDir, Verbose);
          Progress.Update(i + 1);
          Sleep(500); // Simulate work
        end;

        TConsole.WriteLn('All files processed successfully!', ccGreen);
        Result := 0;
      finally
        Progress.Stop;
      end;
    finally
      Files.Free;
      Tags.Free;
    end;
  end;

  procedure TProcessCommand.ProcessFile(const FileName, OutputDir: string;
    const Verbose: Boolean);
  var
    OutputFileName: string;
  begin
    OutputFileName := IncludeTrailingPathDelimiter(OutputDir) +
      ExtractFileName(FileName);
    if Verbose then
      TConsole.WriteLn(Format('Simulating %s -> %s',
        [FileName, OutputFileName]), ccCyan);
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
  Cmd := TProcessCommand.Create('process', 'Process simulated files');
  
  // Basic parameters
  Cmd.AddFlag('-d', '--verbose', 'Show detailed progress');
  Cmd.AddIntegerParameter('-c', '--count', 'Number of files to process', False, '5');
  
  // File and path handling
  Cmd.AddPathParameter('-i', '--input', 'Input directory to process', True);
  Cmd.AddPathParameter('-o', '--output', 'Output directory for results');
  
  // Advanced parameter types
  Cmd.AddEnumParameter('-l', '--log-level', 'Logging verbosity level', 'debug|info|warn|error', False, 'info');
  Cmd.AddArrayParameter('-t', '--tags', 'Tags to apply to processed files');
  Cmd.AddPasswordParameter('-k', '--api-key', 'API key for external service');

  // Register command and run the application
  App.RegisterCommand(Cmd);
  ExitCode := App.Execute;
end.
