{
  Error Handling Demo

  This example demonstrates how to create a command-line application that handles errors
  gracefully. It showcases several key features of the CLI framework:

  1. Error handling (stop-on-error flag)
  2. Command parameters (path and stop-on-error flag)
  3. Colored console output
  4. Basic command structure

  How to run (stop on error):

  $ ErrorHandlingDemo.exe validate -p z:\fake_path -s true
  Validating z:\fake_path\file1.txt... OK
  Validating z:\fake_path\file2.txt... OK
  Validating z:\fake_path\file3.txt... OK
  Validating z:\fake_path\file4.txt... OK
  Validating z:\fake_path\file5.txt... OK
  Validating z:\fake_path\file6.txt... OK
  Validating z:\fake_path\file7.txt... OK
  Validating z:\fake_path\file8.txt... OK
  Validating z:\fake_path\file9.txt... ERROR: Demo validation failed for: z:\fake_path\file9.txt
  Stopping due to error (--stop-on-error)

  How to run (don't stop on error):

  $ ErrorHandlingDemo.exe validate -p z:\fake_path -s false
  Validating z:\fake_path\file1.txt... OK
  Validating z:\fake_path\file2.txt... OK
  Validating z:\fake_path\file3.txt... OK
  Validating z:\fake_path\file4.txt... OK
  Validating z:\fake_path\file5.txt... OK
  Validating z:\fake_path\file6.txt... OK
  Validating z:\fake_path\file7.txt... OK
  Validating z:\fake_path\file8.txt... OK
  Validating z:\fake_path\file9.txt... ERROR: Demo validation failed for: z:\fake_path\file9.txt
  Validating z:\fake_path\file10.txt... OK
  Validation complete with 1 errors
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
  CLI.Parameter,     // Parameter handling
  CLI.Progress,      // Progress indicators
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
  StopOnErrorStr: string; // Raw string value of stop-on-error flag
  StopOnError: Boolean;   // Parsed boolean value of stop-on-error flag
  Files: TStringList;     // List to hold files to validate
  ErrorCount: Integer;    // Tracks number of validation failures
  i: Integer;            // Loop counter
begin
  Result := 0;
  ErrorCount := 0;

  // Get the required path parameter
  // GetParameterValue is a helper method from TBaseCommand
  if not GetParameterValue('--path', Path) then
  begin
    TConsole.WriteLn('Error: Path is required', ccRed);
    Exit(1);
  end;

  // Get stop-on-error flag and convert to boolean
  // This demonstrates handling optional boolean parameters
  StopOnError := False; // Default value
  if GetParameterValue('--stop-on-error', StopOnErrorStr) then
    StopOnError := StrToBoolDef(StopOnErrorStr, False);

  // Create file list and use try-finally for cleanup
  Files := TStringList.Create;
  try
    try
      // In a real app, you would scan the directory here
      // This is just a simulation with hardcoded files
      Files.Add(Path + '\file1.txt');
      Files.Add(Path + '\file2.txt');
      Files.Add(Path + '\file3.txt');
      Files.Add(Path + '\file4.txt');
      Files.Add(Path + '\file5.txt');
      Files.Add(Path + '\file6.txt');
      Files.Add(Path + '\file7.txt');
      Files.Add(Path + '\file8.txt');
      Files.Add(Path + '\file9.txt');
      Files.Add(Path + '\file10.txt');



      // Process each file with error handling
      for i := 0 to Files.Count - 1 do
      begin
        // Show current file being processed
        TConsole.Write('Validating ' + Files[i] + '... ', ccCyan);

        try
          // Attempt to validate the file
          if ValidateFile(Files[i]) then
            TConsole.WriteLn('OK', ccGreen)
          else
          begin
            // Handle validation failure
            TConsole.WriteLn('FAILED', ccRed);
            Inc(ErrorCount);

            // Check if we should stop on first error
            if StopOnError then
            begin
              TConsole.WriteLn('Stopping due to error (--stop-on-error)', ccYellow);
              Exit(1);
            end;
          end;
        except
          // Handle any unexpected exceptions during validation
          on E: Exception do
          begin
            TConsole.WriteLn('ERROR: ' + E.Message, ccRed);
            Inc(ErrorCount);

            if StopOnError then
            begin
              TConsole.WriteLn('Stopping due to error (--stop-on-error)', ccYellow);
              Exit(1);
            end;
          end;
        end;
      end;

      // Show final summary with color-coded output
      if ErrorCount > 0 then
      begin
        TConsole.WriteLn(Format('Validation complete with %d errors', [ErrorCount]), ccYellow);
        Result := 1;
      end
      else
        TConsole.WriteLn('All files validated successfully', ccGreen);

    except
      // Handle any unexpected errors in the main process
      on E: Exception do
      begin
        TConsole.WriteLn('Fatal error: ' + E.Message, ccRed);
        Result := 1;
      end;
    end;
  finally
    // Always clean up resources
    Files.Free;
  end;
end;

function TValidateCommand.ValidateFile(const Path: string): Boolean;
begin
  // Demo: Simulate validation with random failures
  // In a real application, you would:
  // 1. Check if file exists
  // 2. Verify read permissions
  // 3. Validate file contents
  // 4. etc.
  
  Sleep(100);  // Simulate some work
  Result := Random(10) > 4;  // 50% chance of failure
  
  if not Result then
    raise Exception.CreateFmt('Demo validation failed for: %s', [Path]);
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
  Cmd.AddParameter(CreateParameter(
    '-p',            // Short form
    '--path',        // Long form
    'Path to validate', // Description
    True,            // Required parameter
    ptString         // Parameter type: string
  ));

  // Add optional stop-on-error flag
  Cmd.AddParameter(CreateParameter(
    '-s',                          // Short form
    '--stop-on-error',            // Long form
    'Stop processing on first error', // Description
    False,                        // Optional parameter
    ptBoolean,                    // Parameter type: boolean
    'false'                       // Default value
  ));

  // Register command and run the application
  App.RegisterCommand(Cmd);
  ExitCode := App.Execute;
end.
