program SimpleDemo;

{$mode objfpc}{$H+}{$J-}

{ This demo shows how to create a simple CLI application that:
  1. Accepts command-line parameters (--name and --count)
  2. Uses a spinner for visual feedback
  3. Displays colored output
  4. Implements proper error handling }

uses
  SysUtils,         // Basic system utilities (e.g., Sleep, StrToIntDef)
  CLI.Interfaces,   // Core interfaces for CLI components
  CLI.Application,  // Main application framework
  CLI.Parameter,    // Parameter handling and validation
  CLI.Progress,     // Progress indicators (spinner/progress bar)
  CLI.Command,      // Base command implementation
  CLI.Console,      // Colored console output
  CLI.Errors;       // Custom error types

type
  { TGreetCommand - A command that greets someone multiple times
    Inherits from TBaseCommand to get basic command functionality }
  TGreetCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

{ Implementation of the TGreetCommand.Execute method
  Returns:
    0 for successful execution
    non-zero for errors }
function TGreetCommand.Execute: Integer;
var
  NameValue: string;     // Name to greet (from --name parameter)
  CountValue: string;    // Raw count value (from --count parameter)
  Count: Integer;        // Converted count value
  i: Integer;           // Loop counter
  Spinner: IProgressIndicator;  // Progress spinner interface
begin
  Result := 0;  // Initialize to success

  { Get the name parameter value
    If not provided, default to 'World'
    GetParameterValue is inherited from TBaseCommand }
  if not GetParameterValue('--name', NameValue) then
    NameValue := 'World';

  { Get and convert the count parameter
    If not provided or invalid, default to 1
    StrToIntDef safely converts string to integer with a default value }
  if not GetParameterValue('--count', CountValue) then
    Count := 1
  else
    Count := StrToIntDef(CountValue, 1);

  { Create and start a line-style spinner
    The spinner provides visual feedback during the operation }
  Spinner := CreateSpinner(ssLine);
  Spinner.Start;

  { Use try-finally to ensure spinner is properly stopped
    This is important for cleanup, even if an error occurs }
  try
    for i := 1 to Count do
    begin
      Spinner.Update(0);  // Update spinner animation
      Sleep(500);         // Simulate some work (don't use in real code)
      TConsole.WriteLn('Hello, ' + NameValue + '!', ccGreen);  // Green text output
    end;
  finally
    Spinner.Stop;  // Always stop the spinner
  end;
end;

var
  App: ICLIApplication;  // Main application interface
  Cmd: TGreetCommand;   // Our greeting command
begin
  try
    { Create the main application with name and version
      This sets up the basic CLI framework }
    App := CreateCLIApplication('MyApp', '1.0.0');

    { Create and configure the greet command
      - First parameter: command name used in CLI
      - Second parameter: command description for help text }
    Cmd := TGreetCommand.Create('greet', 'Greet someone nicely');

    { Add parameters to the command
      Parameters defined with:
      - Short flag (-n)
      - Long flag (--name)
      - Description
      - Required flag (False = optional)
      - Parameter type (string/integer)
      - Default value }
    Cmd.AddStringParameter('-n', '--name', 'Name to greet', False, 'World');
    Cmd.AddIntegerParameter('-c', '--count', 'Number of times to greet', False, '1');

    { Register the command with the application
      This makes it available for use }
    App.RegisterCommand(Cmd);
    Cmd := nil;  // App takes ownership, clear our reference

    { Execute the application and store exit code
      This processes command line and runs the appropriate command }
    ExitCode := App.Execute;
  except
    { Handle any unhandled exceptions
      Display error in red and set error exit code }
    on E: Exception do
    begin
      TConsole.WriteLn('Error: ' + E.Message, ccRed);
      ExitCode := 1;
    end;
  end;
end.