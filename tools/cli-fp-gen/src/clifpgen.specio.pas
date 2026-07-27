unit CliFpGen.SpecIO;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, fpjson, jsonparser,
  CliFpGen.Model;

function LoadProjectSpec(const SpecFile: string): TProjectSpec;
procedure SaveProjectSpec(const Spec: TProjectSpec; const SpecFile: string; const Options: TWriteOptions);

implementation

uses
  CliFpGen.Filesystem;

function RequireObjectField(const Obj: TJSONObject; const Name: string): TJSONObject;
begin
  if (Obj = nil) or (Obj.Find(Name) = nil) or not (Obj.Objects[Name] is TJSONObject) then
    raise Exception.CreateFmt('Invalid spec: missing object field "%s"', [Name]);
  Result := Obj.Objects[Name];
end;

function LoadParameterSpec(const ParamObj: TJSONObject;
  const Location: string): TParameterSpec;
var
  ParamKind: TParameterKind;
begin
  Result := TParameterSpec.Create;
  try
    Result.ShortFlag := ParamObj.Get('short', '');
    Result.LongFlag := ParamObj.Get('long', '');
    Result.Description := ParamObj.Get('description', '');
    if not TryParseParameterKind(ParamObj.Get('kind', 'string'), ParamKind) then
      raise Exception.CreateFmt('Invalid parameter kind in %s', [Location]);
    Result.Kind := ParamKind;
    Result.Required := ParamObj.Get('required', False);
    Result.DefaultValue := ParamObj.Get('default', '');
    Result.AllowedValues := ParamObj.Get('allowedValues', '');
  except
    Result.Free;
    Result := nil;
    raise;
  end;
end;

procedure LoadParameters(const OwnerObj: TJSONObject;
  const Location: string; const Parameters: TParameterSpecList);
var
  ParamArray: TJSONArray;
  ParamObj: TJSONObject;
  Param: TParameterSpec;
  ParamLocation: string;
  j: Integer;
begin
  if OwnerObj.Find('parameters') = nil then
    Exit;
  if not (OwnerObj.Find('parameters') is TJSONArray) then
    raise Exception.CreateFmt('Invalid spec: %s.parameters must be an array',
      [Location]);

  ParamArray := TJSONArray(OwnerObj.Find('parameters'));
  for j := 0 to ParamArray.Count - 1 do
  begin
    ParamLocation := Format('%s.parameters[%d]', [Location, j]);
    if not (ParamArray.Items[j] is TJSONObject) then
      raise Exception.CreateFmt('Invalid spec: %s must be an object',
        [ParamLocation]);
    ParamObj := TJSONObject(ParamArray.Items[j]);
    Param := LoadParameterSpec(ParamObj, ParamLocation);
    try
      // Parameters is owning; clear the local only after Add succeeds.
      Parameters.Add(Param);
      Param := nil;
    finally
      Param.Free;
    end;
  end;
end;

function LoadCommandSpec(const CmdObj: TJSONObject;
  const CommandIndex: Integer): TCommandSpec;
begin
  Result := TCommandSpec.Create;
  try
    Result.Name := CmdObj.Get('name', '');
    Result.Description := CmdObj.Get('description', '');
    Result.ParentPath := CmdObj.Get('parent', '');
    LoadParameters(CmdObj, Format('commands[%d]', [CommandIndex]),
      Result.Parameters);
  except
    Result.Free;
    Result := nil;
    raise;
  end;
end;

function LoadProjectSpec(const SpecFile: string): TProjectSpec;
var
  Root: TJSONData;
  RootObj, AppObj, RootCommandObj: TJSONObject;
  CmdArray: TJSONArray;
  i: Integer;
  CmdObj: TJSONObject;
  Cmd: TCommandSpec;
  JsonText: string;
begin
  if not FileExists(SpecFile) then
    raise Exception.CreateFmt('Spec file not found: %s', [SpecFile]);

  JsonText := ReadTextFileStrict(SpecFile);
  Root := GetJSON(JsonText);
  try
    if not (Root is TJSONObject) then
      raise Exception.Create('Invalid spec: root must be a JSON object');
    RootObj := TJSONObject(Root);

    Result := TProjectSpec.Create;
    try
      Result.SchemaVersion := RootObj.Get('schemaVersion', 1);

      AppObj := RequireObjectField(RootObj, 'app');
      Result.AppName := AppObj.Get('name', '');
      Result.AppVersion := AppObj.Get('version', '0.1.0');
      Result.ProgramFile := AppObj.Get('programFile', '');

      if RootObj.Find('rootCommand') <> nil then
      begin
        if not (RootObj.Find('rootCommand') is TJSONObject) then
          raise Exception.Create(
            'Invalid spec: rootCommand must be an object');
        RootCommandObj := TJSONObject(RootObj.Find('rootCommand'));
        Result.HasRootCommand := True;
        Result.RootCommand.Description :=
          RootCommandObj.Get('description', '');
        LoadParameters(RootCommandObj, 'rootCommand',
          Result.RootCommand.Parameters);
      end;

      if (RootObj.Find('commands') <> nil) and (RootObj.Arrays['commands'] is TJSONArray) then
      begin
        CmdArray := RootObj.Arrays['commands'];
        for i := 0 to CmdArray.Count - 1 do
        begin
          if not (CmdArray.Items[i] is TJSONObject) then
            raise Exception.CreateFmt('Invalid spec: commands[%d] must be an object', [i]);
          CmdObj := TJSONObject(CmdArray.Items[i]);
          Cmd := LoadCommandSpec(CmdObj, i);
          try
            // Commands is owning; clear the local only after Add succeeds.
            Result.Commands.Add(Cmd);
            Cmd := nil;
          finally
            Cmd.Free;
          end;
        end;
      end;
    except
      Result.Free;
      raise;
    end;
  finally
    Root.Free;
  end;
end;

procedure SaveProjectSpec(const Spec: TProjectSpec; const SpecFile: string; const Options: TWriteOptions);
var
  RootObj, AppObj, RootCommandObj, CmdObj, ParamObj: TJSONObject;
  CmdArray, ParamArray: TJSONArray;
  i: Integer;
  j: Integer;
  Cmd: TCommandSpec;
  Param: TParameterSpec;
begin
  RootObj := TJSONObject.Create;
  try
    RootObj.Add('schemaVersion', Spec.SchemaVersion);

    AppObj := TJSONObject.Create;
    AppObj.Add('name', Spec.AppName);
    AppObj.Add('version', Spec.AppVersion);
    AppObj.Add('programFile', Spec.ProgramFile);
    RootObj.Add('app', AppObj);

    if Spec.HasRootCommand then
    begin
      RootCommandObj := TJSONObject.Create;
      RootCommandObj.Add('description', Spec.RootCommand.Description);
      ParamArray := TJSONArray.Create;
      for j := 0 to Spec.RootCommand.Parameters.Count - 1 do
      begin
        Param := Spec.RootCommand.Parameters[j];
        ParamObj := TJSONObject.Create;
        ParamObj.Add('kind', ParameterKindToString(Param.Kind));
        ParamObj.Add('short', Param.ShortFlag);
        ParamObj.Add('long', Param.LongFlag);
        ParamObj.Add('description', Param.Description);
        ParamObj.Add('required', Param.Required);
        ParamObj.Add('default', Param.DefaultValue);
        ParamObj.Add('allowedValues', Param.AllowedValues);
        ParamArray.Add(ParamObj);
      end;
      RootCommandObj.Add('parameters', ParamArray);
      RootObj.Add('rootCommand', RootCommandObj);
    end;

    CmdArray := TJSONArray.Create;
    for i := 0 to Spec.Commands.Count - 1 do
    begin
      Cmd := Spec.Commands[i];
      CmdObj := TJSONObject.Create;
      CmdObj.Add('name', Cmd.Name);
      CmdObj.Add('description', Cmd.Description);
      if Trim(Cmd.ParentPath) <> '' then
        CmdObj.Add('parent', Cmd.ParentPath)
      else
        CmdObj.Add('parent', '');

      ParamArray := TJSONArray.Create;
      for j := 0 to Cmd.Parameters.Count - 1 do
      begin
        Param := Cmd.Parameters[j];
        ParamObj := TJSONObject.Create;
        ParamObj.Add('kind', ParameterKindToString(Param.Kind));
        ParamObj.Add('short', Param.ShortFlag);
        ParamObj.Add('long', Param.LongFlag);
        ParamObj.Add('description', Param.Description);
        ParamObj.Add('required', Param.Required);
        ParamObj.Add('default', Param.DefaultValue);
        ParamObj.Add('allowedValues', Param.AllowedValues);
        ParamArray.Add(ParamObj);
      end;
      CmdObj.Add('parameters', ParamArray);
      CmdArray.Add(CmdObj);
    end;
    RootObj.Add('commands', CmdArray);

    WriteManagedTextFile(SpecFile, RootObj.FormatJSON([], 2) + LineEnding, wkGenerated, Options);
  finally
    RootObj.Free;
  end;
end;

end.
