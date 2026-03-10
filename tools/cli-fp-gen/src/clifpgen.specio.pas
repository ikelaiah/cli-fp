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

function LoadProjectSpec(const SpecFile: string): TProjectSpec;
var
  Root: TJSONData;
  RootObj, AppObj: TJSONObject;
  CmdArray, ParamArray: TJSONArray;
  i: Integer;
  j: Integer;
  CmdObj, ParamObj: TJSONObject;
  Cmd: TCommandSpec;
  Param: TParameterSpec;
  ParamKind: TParameterKind;
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

      if (RootObj.Find('commands') <> nil) and (RootObj.Arrays['commands'] is TJSONArray) then
      begin
        CmdArray := RootObj.Arrays['commands'];
        for i := 0 to CmdArray.Count - 1 do
        begin
          if not (CmdArray.Items[i] is TJSONObject) then
            raise Exception.CreateFmt('Invalid spec: commands[%d] must be an object', [i]);
          CmdObj := TJSONObject(CmdArray.Items[i]);
          Cmd := TCommandSpec.Create;
          Cmd.Name := CmdObj.Get('name', '');
          Cmd.Description := CmdObj.Get('description', '');
          Cmd.ParentPath := CmdObj.Get('parent', '');
          if (CmdObj.Find('parameters') <> nil) and (CmdObj.Arrays['parameters'] is TJSONArray) then
          begin
            ParamArray := CmdObj.Arrays['parameters'];
            for j := 0 to ParamArray.Count - 1 do
            begin
              if not (ParamArray.Items[j] is TJSONObject) then
                raise Exception.CreateFmt('Invalid spec: commands[%d].parameters[%d] must be an object', [i, j]);
              ParamObj := TJSONObject(ParamArray.Items[j]);
              Param := TParameterSpec.Create;
              Param.ShortFlag := ParamObj.Get('short', '');
              Param.LongFlag := ParamObj.Get('long', '');
              Param.Description := ParamObj.Get('description', '');
              if not TryParseParameterKind(ParamObj.Get('kind', 'string'), ParamKind) then
                raise Exception.CreateFmt('Invalid parameter kind in commands[%d].parameters[%d]', [i, j]);
              Param.Kind := ParamKind;
              Param.Required := ParamObj.Get('required', False);
              Param.DefaultValue := ParamObj.Get('default', '');
              Param.AllowedValues := ParamObj.Get('allowedValues', '');
              Cmd.Parameters.Add(Param);
            end;
          end;
          Result.Commands.Add(Cmd);
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
  RootObj, AppObj, CmdObj, ParamObj: TJSONObject;
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
