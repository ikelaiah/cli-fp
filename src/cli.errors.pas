unit CLI.Errors;

{$mode objfpc}{$H+}{$J-}

{ This unit defines the exception hierarchy for the CLI framework.
  It provides specific exception types for different error scenarios,
  allowing for precise error handling and appropriate user feedback.
  
  Usage example:
    raise ECommandNotFoundException.Create('Unknown command: ' + CommandName);
    raise ERequiredParameterMissingException.CreateFmt('Required parameter "%s" not provided', [ParamName]); }

interface

uses
  Classes, SysUtils;

type
  { Base exception class for all CLI-related errors
    Use this as the base class for catching any CLI framework error
    @member Create Creates exception with message
    @member CreateFmt Creates exception with formatted message }
  ECLIException = class(Exception)
  public
    constructor Create(const Msg: string); override;
    constructor CreateFmt(const Msg: string; const Args: array of const); override;
  end;

  { Raised when a command cannot be found
    Examples:
    - Unknown command name
    - Unknown subcommand
    - Command not registered with application
    @member Create Creates exception with message
    @member CreateFmt Creates exception with formatted message }
  ECommandNotFoundException = class(ECLIException)
  public
    constructor Create(const Msg: string); override;
    constructor CreateFmt(const Msg: string; const Args: array of const); override;
  end;

  { Raised when a parameter is invalid
    Examples:
    - Unknown parameter flag
    - Duplicate parameter
    - Invalid parameter format
    @member Create Creates exception with message
    @member CreateFmt Creates exception with formatted message }
  EInvalidParameterException = class(ECLIException)
  public
    constructor Create(const Msg: string); override;
    constructor CreateFmt(const Msg: string; const Args: array of const); override;
  end;

  { Raised when a required parameter is not provided
    Examples:
    - Missing mandatory parameter
    - Required parameter has no value
    - Required parameter has no default
    @member Create Creates exception with message
    @member CreateFmt Creates exception with formatted message }
  ERequiredParameterMissingException = class(ECLIException)
  public
    constructor Create(const Msg: string); override;
    constructor CreateFmt(const Msg: string; const Args: array of const); override;
  end;

  { Raised when a parameter value is invalid
    Examples:
    - Integer parameter contains non-numeric value
    - Float parameter has invalid format
    - Value outside allowed range
    @member Create Creates exception with message
    @member CreateFmt Creates exception with formatted message }
  EInvalidParameterValueException = class(ECLIException)
  public
    constructor Create(const Msg: string); override;
    constructor CreateFmt(const Msg: string; const Args: array of const); override;
  end;

  { Raised when command execution fails
    Examples:
    - Runtime errors during command execution
    - Resource access failures
    - Business logic errors
    @member Create Creates exception with message
    @member CreateFmt Creates exception with formatted message }
  ECommandExecutionException = class(ECLIException)
  public
    constructor Create(const Msg: string); override;
    constructor CreateFmt(const Msg: string; const Args: array of const); override;
  end;

implementation

{ ECLIException }

constructor ECLIException.Create(const Msg: string);
begin
  inherited Create(Msg);
end;

constructor ECLIException.CreateFmt(const Msg: string; const Args: array of const);
begin
  inherited CreateFmt(Msg, Args);
end;

{ ECommandNotFoundException }

constructor ECommandNotFoundException.Create(const Msg: string);
begin
  inherited Create(Msg);
end;

constructor ECommandNotFoundException.CreateFmt(const Msg: string; const Args: array of const);
begin
  inherited CreateFmt(Msg, Args);
end;

{ EInvalidParameterException }

constructor EInvalidParameterException.Create(const Msg: string);
begin
  inherited Create(Msg);
end;

constructor EInvalidParameterException.CreateFmt(const Msg: string; const Args: array of const);
begin
  inherited CreateFmt(Msg, Args);
end;

{ ERequiredParameterMissingException }

constructor ERequiredParameterMissingException.Create(const Msg: string);
begin
  inherited Create(Msg);
end;

constructor ERequiredParameterMissingException.CreateFmt(const Msg: string; const Args: array of const);
begin
  inherited CreateFmt(Msg, Args);
end;

{ EInvalidParameterValueException }

constructor EInvalidParameterValueException.Create(const Msg: string);
begin
  inherited Create(Msg);
end;

constructor EInvalidParameterValueException.CreateFmt(const Msg: string; const Args: array of const);
begin
  inherited CreateFmt(Msg, Args);
end;

{ ECommandExecutionException }

constructor ECommandExecutionException.Create(const Msg: string);
begin
  inherited Create(Msg);
end;

constructor ECommandExecutionException.CreateFmt(const Msg: string; const Args: array of const);
begin
  inherited CreateFmt(Msg, Args);
end;

end.
