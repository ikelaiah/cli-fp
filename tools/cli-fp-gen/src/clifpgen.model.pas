unit CliFpGen.Model;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, fgl;

type
  TParameterKind = (
    pkString,
    pkInteger,
    pkFloat,
    pkFlag,
    pkBoolean,
    pkPath,
    pkEnum,
    pkDateTime,
    pkArray,
    pkPassword,
    pkUrl
  );

  TParameterSpec = class
  private
    FShortFlag: string;
    FLongFlag: string;
    FDescription: string;
    FKind: TParameterKind;
    FRequired: Boolean;
    FDefaultValue: string;
    FAllowedValues: string;
  public
    constructor Create;
    property ShortFlag: string read FShortFlag write FShortFlag;
    property LongFlag: string read FLongFlag write FLongFlag;
    property Description: string read FDescription write FDescription;
    property Kind: TParameterKind read FKind write FKind;
    property Required: Boolean read FRequired write FRequired;
    property DefaultValue: string read FDefaultValue write FDefaultValue;
    property AllowedValues: string read FAllowedValues write FAllowedValues;
  end;

  TParameterSpecList = specialize TFPGObjectList<TParameterSpec>;

  TCommandSpec = class
  private
    FName: string;
    FDescription: string;
    FParentPath: string;
    FParameters: TParameterSpecList;
  public
    constructor Create;
    destructor Destroy; override;
    property Name: string read FName write FName;
    property Description: string read FDescription write FDescription;
    property ParentPath: string read FParentPath write FParentPath; // slash-delimited, empty for root
    property Parameters: TParameterSpecList read FParameters;
  end;

  TCommandSpecList = specialize TFPGObjectList<TCommandSpec>;

  TProjectSpec = class
  private
    FSchemaVersion: Integer;
    FAppName: string;
    FAppVersion: string;
    FProgramFile: string;
    FCommands: TCommandSpecList;
  public
    constructor Create;
    destructor Destroy; override;
    property SchemaVersion: Integer read FSchemaVersion write FSchemaVersion;
    property AppName: string read FAppName write FAppName;
    property AppVersion: string read FAppVersion write FAppVersion;
    property ProgramFile: string read FProgramFile write FProgramFile;
    property Commands: TCommandSpecList read FCommands;
  end;

  TWriteOptions = record
    DryRun: Boolean;
    Force: Boolean;
  end;

function CommandFullPath(const Command: TCommandSpec): string;
function ParameterKindToString(const Kind: TParameterKind): string;
function TryParseParameterKind(const S: string; out Kind: TParameterKind): Boolean;

implementation

function CommandFullPath(const Command: TCommandSpec): string;
begin
  if Trim(Command.ParentPath) = '' then
    Result := Command.Name
  else
    Result := Command.ParentPath + '/' + Command.Name;
end;

constructor TParameterSpec.Create;
begin
  inherited Create;
  FKind := pkString;
  FRequired := False;
end;

constructor TCommandSpec.Create;
begin
  inherited Create;
  FParameters := TParameterSpecList.Create(True);
end;

destructor TCommandSpec.Destroy;
begin
  FParameters.Free;
  inherited Destroy;
end;

function ParameterKindToString(const Kind: TParameterKind): string;
begin
  case Kind of
    pkString:   Result := 'string';
    pkInteger:  Result := 'integer';
    pkFloat:    Result := 'float';
    pkFlag:     Result := 'flag';
    pkBoolean:  Result := 'boolean';
    pkPath:     Result := 'path';
    pkEnum:     Result := 'enum';
    pkDateTime: Result := 'datetime';
    pkArray:    Result := 'array';
    pkPassword: Result := 'password';
    pkUrl:      Result := 'url';
  else
    Result := 'string';
  end;
end;

function TryParseParameterKind(const S: string; out Kind: TParameterKind): Boolean;
var
  V: string;
begin
  V := LowerCase(Trim(S));
  Result := True;
  if V = 'string' then Kind := pkString
  else if V = 'integer' then Kind := pkInteger
  else if V = 'float' then Kind := pkFloat
  else if V = 'flag' then Kind := pkFlag
  else if V = 'boolean' then Kind := pkBoolean
  else if V = 'path' then Kind := pkPath
  else if V = 'enum' then Kind := pkEnum
  else if (V = 'datetime') or (V = 'date-time') then Kind := pkDateTime
  else if V = 'array' then Kind := pkArray
  else if V = 'password' then Kind := pkPassword
  else if V = 'url' then Kind := pkUrl
  else
    Result := False;
end;

constructor TProjectSpec.Create;
begin
  inherited Create;
  FSchemaVersion := 1;
  FAppVersion := '0.1.0';
  FCommands := TCommandSpecList.Create(True);
end;

destructor TProjectSpec.Destroy;
begin
  FCommands.Free;
  inherited Destroy;
end;

end.
