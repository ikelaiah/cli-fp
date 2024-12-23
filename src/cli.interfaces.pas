unit CLI.Interfaces;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils;

type
  { Parameter types that can be used in command-line arguments }
  TParameterType = (
    ptString,   // String value (e.g., --name "John Doe")
    ptInteger,  // Integer value (e.g., --count 42)
    ptFloat,    // Float value (e.g., --rate 3.14)
    ptBoolean,  // Boolean value (e.g., --verbose) or flag (e.g., --force)
    ptPath,     // File/directory path (e.g., --input /path/to/file)
    ptEnum,     // Enumerated value (e.g., --log-level debug|info|warn|error)
    ptDateTime, // Date/time value (e.g., --start "2024-01-01 12:00")
    ptArray,    // Comma-separated list (e.g., --tags tag1,tag2,tag3)
    ptPassword, // Sensitive value, masked in help/logs (e.g., --api-key ***)
    ptUrl       // URL value with format validation (e.g., --repo https://github.com/user/repo)
  );

  { Forward declarations for circular references }
  ICommand = interface;
  ICommandParameter = interface;
  ICLIApplication = interface;
  IProgressIndicator = interface;

  { Command interface - Represents a CLI command or subcommand }
  ICommand = interface
    ['{D6F6D6D0-5C5C-4B5B-9B5B-5B5B5B5B5B5B}']
    { Returns the command name as used in the CLI (e.g., 'clone' in 'git clone') }
    function GetName: string;
    
    { Returns the command description shown in help text }
    function GetDescription: string;
    
    { Returns array of parameters accepted by this command
      @returns Array of ICommandParameter representing command's parameters }
    function GetParameters: specialize TArray<ICommandParameter>;
    
    { Returns array of subcommands under this command
      @returns Array of ICommand representing nested subcommands }
    function GetSubCommands: specialize TArray<ICommand>;
    
    { Executes the command with current parameters
      @returns Integer exit code (0 for success, non-zero for error) }
    function Execute: Integer;
    
    { Command name as used in CLI }
    property Name: string read GetName;
    
    { Command description for help text }
    property Description: string read GetDescription;
    
    { Command parameters }
    property Parameters: specialize TArray<ICommandParameter> read GetParameters;
    
    { Nested subcommands }
    property SubCommands: specialize TArray<ICommand> read GetSubCommands;
  end;

  { Command parameter interface - Represents a single command-line parameter }
  ICommandParameter = interface
    ['{D6F6D6D1-5C5C-4B5B-9B5B-5B5B5B5B5B5B}']
    { Returns short form flag (e.g., '-n') }
    function GetShortFlag: string;
    
    { Returns long form flag (e.g., '--name') }
    function GetLongFlag: string;
    
    { Returns parameter description for help text }
    function GetDescription: string;
    
    { Returns whether parameter is required
      @returns True if parameter must be provided, False if optional }
    function GetRequired: Boolean;
    
    { Returns parameter's data type }
    function GetParamType: TParameterType;
    
    { Returns default value if parameter is optional }
    function GetDefaultValue: string;
    
    { Returns allowed values for the parameter }
    function GetAllowedValues: string;
    
    { Short form flag (e.g., '-n') }
    property ShortFlag: string read GetShortFlag;
    
    { Long form flag (e.g., '--name') }
    property LongFlag: string read GetLongFlag;
    
    { Parameter description }
    property Description: string read GetDescription;
    
    { Whether parameter is required }
    property Required: Boolean read GetRequired;
    
    { Parameter's data type }
    property ParamType: TParameterType read GetParamType;
    
    { Default value for optional parameters }
    property DefaultValue: string read GetDefaultValue;
    
    { Allowed values for the parameter }
    property AllowedValues: string read GetAllowedValues;
  end;

  { Progress indicator interface - For showing progress during long operations }
  IProgressIndicator = interface
    ['{C3D4E5F6-A7B8-49C0-D1E2-F3A4B5C6D7E8}']
    { Starts the progress indicator animation/display }
    procedure Start;
    
    { Stops the progress indicator animation/display }
    procedure Stop;
    
    { Updates the progress percentage
      @param Progress Integer between 0-100 representing completion percentage }
    procedure Update(const Progress: Integer);
  end;

  { CLI application interface - Main application controller }
  ICLIApplication = interface
    ['{D6F6D6D2-5C5C-4B5B-9B5B-5B5B5B5B5B5B}']
    { Registers a command with the application
      @param Command The command to register
      @raises Exception if command with same name already exists }
    procedure RegisterCommand(const Command: ICommand);
    
    { Executes the application, parsing command line and running appropriate command
      @returns Integer exit code (0 for success, non-zero for error) }
    function Execute: Integer;
  end;

implementation

end. 
