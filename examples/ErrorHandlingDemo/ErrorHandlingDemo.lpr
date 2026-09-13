{
  Error Handling Demo

  This example demonstrates how to create a command-line application that handles errors
  gracefully. It showcases several key features of the CLI framework:

  1. Error handling (stop-on-error flag)
  2. Command parameters (path and stop-on-error flag)
  3. Colored console output
  4. Basic command structure

  How to run (stop at the first simulated failure):

  $ ErrorHandlingDemo.exe validate --path . --stop-on-error

  The fifth of ten simulated files deterministically fails validation. With
  --stop-on-error, processing stops at that failure; without it, every file is
  processed and the command returns a non-zero exit code after its summary.
}
program ErrorHandlingDemo;

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
  CLI.Console;       // Colored output

type
  { TValidateCommand - Command class that validates files
    This demonstrates error handling patterns and parameter usage.
    The command takes a path and validates files in it, with options
    to stop on first error. }
  TValidateCommand = class(TBaseCommand)
  private
    { Simulates validating a single file
      @param Path The file path to validate
      @return True if validation passed, False if failed }
    function ValidateFile(const Path: string): Boolean;
  public
    { Main execution method that runs the validation process
      @return 0 for success, 1 for any errors }
    function Execute: Integer; override;
  end;

function TValidateCommand.Execute: Integer;
var
  Path: string;           // Path parameter from command line
  StopOnErrorStr: string;
  StopOnError: Boolean;
  Files: TStringList;
  ErrorCount: Integer;
  i: Integer;
begin
  Result := 0;
  ErrorCount := 0;

  if not GetParameterValue('--path', Path) then
  begin
    TConsole.WriteLn('Error: Path is required', ccRed);
    Exit(1);
  end;

  // AddFlag supplies "false" when the flag is absent.
  GetParameterValue('--stop-on-error', StopOnErrorStr);
  StopOnError := SameText(StopOnErrorStr, 'true');

  Files := TStringList.Create;
  try
    // In an application this list could come from a directory scan. The fixed
    // sample set makes the error-handling output repeatable for learners.
    for i := 1 to 10 do
      Files.Add(IncludeTrailingPathDelimiter(Path) +
        Format('file%d.txt', [i]));

    for i := 0 to Files.Count - 1 do
    begin
      TConsole.Write('Validating ' + Files[i] + '... ', ccCyan);
      if ValidateFile(Files[i]) then
        TConsole.WriteLn('OK', ccGreen)
      else
      begin
        TConsole.WriteLn('FAILED', ccRed);
        Inc(ErrorCount);
        if StopOnError then
        begin
          TConsole.WriteLn('Stopping due to error (--stop-on-error)', ccYellow);
          Exit(1);
        end;
      end
    end;

    if ErrorCount > 0 then
    begin
      TConsole.WriteLn(Format('Validation complete with %d errors', [ErrorCount]), ccYellow);
      Result := 1;
    end
    else
      TConsole.WriteLn('All files validated successfully', ccGreen);
  finally
    Files.Free;
  end;
end;

function TValidateCommand.ValidateFile(const Path: string): Boolean;
begin
  Sleep(50); // Simulate validation work without making the example flaky.
  Result := not SameText(ExtractFileName(Path), 'file5.txt');
end;

// Main program setup
var
  App: ICLIApplication;
  Cmd: TValidateCommand;
begin
  // Create the main application with name and version
  App := CreateCLIApplication('MyApp', '1.0.0');

  // Create the validate command
  Cmd := TValidateCommand.Create('validate', 'Validate files');

  // Add required path parameter
  Cmd.AddPathParameter(
    '-p',            // Short form
    '--path',        // Long form
    'Path to validate', // Description
    True             // Required parameter
  );

  // Add optional stop-on-error flag
  Cmd.AddFlag(
    '-s',                          // Short form
    '--stop-on-error',            // Long form
    'Stop processing on first error' // Description
  );

  // Return the framework's execution result to the operating system.
  App.RegisterCommand(Cmd);
  Halt(App.Execute);
end.
