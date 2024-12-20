program MyApp;

{$mode objfpc}  // Use Object Pascal mode
{$H+}           // Use AnsiStrings instead of ShortStrings
{$J-}           // Disable writeable typed constants

uses
  SysUtils,           // Basic system utilities
  CLI.Interfaces,     // Interfaces for CLI framework
  CLI.Application,    // Main CLI application functionality
  CLI.Command;        // Base command implementations

type
  { TGreetCommand - A simple command that displays a greeting }
  TGreetCommand = class(TBaseCommand)  // Inherit from TBaseCommand
  public
    function Execute: Integer; override;  // Override the Execute method
  end;

{ Implementation of the TGreetCommand.Execute method
  Returns: 0 for successful execution }
function TGreetCommand.Execute: Integer;
begin
  WriteLn('Hello, CLI World!');  // Display greeting message
  Result := 0;                   // Return success code
end;

var
  App: ICLIApplication;  // Interface reference to our CLI application
  Cmd: TGreetCommand;    // Instance of our greeting command
begin
  // Create a new CLI application with name and version
  App := CreateCLIApplication('MyApp', '1.0.0');
  
  // Create a new command instance with name and description
  Cmd := TGreetCommand.Create('greet', 'Say hello');
  
  // Register the command with the application
  App.RegisterCommand(Cmd);
  
  // Execute the application and store the exit code
  ExitCode := App.Execute;
end.
