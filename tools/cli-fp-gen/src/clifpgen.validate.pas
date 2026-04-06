unit CliFpGen.Validate;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, CliFpGen.Model;

procedure ValidateProjectSpec(const Spec: TProjectSpec);

implementation

uses
  CliFpGen.Naming;

function StartsWith(const S, Prefix: string): Boolean;
begin
  Result := Copy(S, 1, Length(Prefix)) = Prefix;
end;

function IsAbsoluteLikePath(const S: string): Boolean;
begin
  Result := (ExtractFileDrive(S) <> '') or
    ((S <> '') and ((S[1] = '/') or (S[1] = '\')));
end;

function ContainsParentTraversal(const S: string): Boolean;
var
  Parts: TStringList;
  i: Integer;
begin
  Result := False;
  Parts := TStringList.Create;
  try
    Parts.Delimiter := '/';
    Parts.StrictDelimiter := True;
    Parts.DelimitedText := S;
    for i := 0 to Parts.Count - 1 do
      if Trim(Parts[i]) = '..' then
        Exit(True);
  finally
    Parts.Free;
  end;
end;

procedure ValidateProjectSpec(const Spec: TProjectSpec);
var
  SeenPaths, RootPaths: TStringList;
  i: Integer;
  j: Integer;
  Cmd: TCommandSpec;
  FullPath, ParentPath: string;
  Param: TParameterSpec;
  SeenFlags: TStringList;
  ProgramFileNorm: string;
begin
  if Spec = nil then
    raise Exception.Create('Spec is nil');

  // TODO: when v2 is defined, add migration guidance to this error message
  if Spec.SchemaVersion <> 1 then
    raise Exception.CreateFmt(
      'Unsupported schemaVersion: %d (only v1 is supported). ' +
      'Check the cli-fp-gen documentation for migration instructions.',
      [Spec.SchemaVersion]);

  if Trim(Spec.AppName) = '' then
    raise Exception.Create('Spec app.name must not be empty');

  if Trim(Spec.ProgramFile) = '' then
    raise Exception.Create('Spec app.programFile must not be empty');

  ProgramFileNorm := NormalizePathSlashes(Trim(Spec.ProgramFile));
  if IsAbsoluteLikePath(ProgramFileNorm) then
    raise Exception.CreateFmt('Spec app.programFile must be project-relative: %s', [Spec.ProgramFile]);
  if ContainsParentTraversal(ProgramFileNorm) then
    raise Exception.CreateFmt('Spec app.programFile must not escape the project directory: %s', [Spec.ProgramFile]);
  if not StartsWith(AnsiLowerCase(ProgramFileNorm), 'src/') then
    raise Exception.CreateFmt('Spec app.programFile must live under src/: %s', [Spec.ProgramFile]);
  if LowerCase(ExtractFileExt(ProgramFileNorm)) <> '.lpr' then
    raise Exception.CreateFmt('Spec app.programFile must be an .lpr file: %s', [Spec.ProgramFile]);

  SeenPaths := TStringList.Create;
  RootPaths := TStringList.Create;
  try
    SeenPaths.CaseSensitive := False;
    RootPaths.CaseSensitive := False;

    for i := 0 to Spec.Commands.Count - 1 do
    begin
      Cmd := Spec.Commands[i];
      Cmd.Name := NormalizeCommandName(Cmd.Name);
      Cmd.ParentPath := NormalizeCommandPath(Cmd.ParentPath);

      if Cmd.Description = '' then
        Cmd.Description := 'TODO: Describe "' + Cmd.Name + '" command';

      if not IsValidCommandToken(Cmd.Name) then
        raise Exception.CreateFmt('Invalid command name "%s"', [Cmd.Name]);

      if Cmd.ParentPath <> '' then
      begin
        ParentPath := NormalizeCommandPath(Cmd.ParentPath);
        if ParentPath = '' then
          raise Exception.CreateFmt('Invalid parent path for command "%s"', [Cmd.Name]);
      end;

      FullPath := CommandFullPath(Cmd);
      if SeenPaths.IndexOf(AnsiLowerCase(FullPath)) >= 0 then
      begin
        if Cmd.ParentPath <> '' then
          raise Exception.CreateFmt(
            'Duplicate command name "%s" under parent "%s" (full path: "%s")',
            [Cmd.Name, Cmd.ParentPath, FullPath])
        else
          raise Exception.CreateFmt(
            'Duplicate root command name "%s"', [Cmd.Name]);
      end;
      SeenPaths.Add(AnsiLowerCase(FullPath));

      SeenFlags := TStringList.Create;
      try
        SeenFlags.CaseSensitive := False;
        for j := 0 to Cmd.Parameters.Count - 1 do
        begin
          Param := Cmd.Parameters[j];

          if Trim(Param.LongFlag) = '' then
            raise Exception.CreateFmt('Command "%s": parameter %d missing long flag', [FullPath, j]);
          if not StartsWith(Param.LongFlag, '--') then
            raise Exception.CreateFmt('Command "%s": invalid long flag "%s"', [FullPath, Param.LongFlag]);
          if (Trim(Param.ShortFlag) <> '') and (not StartsWith(Param.ShortFlag, '-')) then
            raise Exception.CreateFmt('Command "%s": invalid short flag "%s"', [FullPath, Param.ShortFlag]);
          if StartsWith(Param.ShortFlag, '--') then
            raise Exception.CreateFmt('Command "%s": short flag must be single-dash style ("%s")', [FullPath, Param.ShortFlag]);

          if Trim(Param.Description) = '' then
            Param.Description := 'TODO: Describe ' + Param.LongFlag;

          if Param.Kind = pkFlag then
          begin
            Param.Required := False;
            if Param.DefaultValue = '' then
              Param.DefaultValue := 'false';
          end;

          if (Param.Kind = pkEnum) and (Trim(Param.AllowedValues) = '') then
            raise Exception.CreateFmt('Command "%s": enum parameter "%s" requires allowedValues',
              [FullPath, Param.LongFlag]);

          if SeenFlags.IndexOf(AnsiLowerCase(Param.LongFlag)) >= 0 then
            raise Exception.CreateFmt('Command "%s": duplicate parameter flag "%s"', [FullPath, Param.LongFlag]);
          SeenFlags.Add(AnsiLowerCase(Param.LongFlag));

          if Trim(Param.ShortFlag) <> '' then
          begin
            if SeenFlags.IndexOf(AnsiLowerCase(Param.ShortFlag)) >= 0 then
              raise Exception.CreateFmt('Command "%s": duplicate parameter flag "%s"', [FullPath, Param.ShortFlag]);
            SeenFlags.Add(AnsiLowerCase(Param.ShortFlag));
          end;
        end;
      finally
        SeenFlags.Free;
      end;
    end;

    for i := 0 to Spec.Commands.Count - 1 do
    begin
      Cmd := Spec.Commands[i];
      if Cmd.ParentPath <> '' then
      begin
        if SeenPaths.IndexOf(AnsiLowerCase(Cmd.ParentPath)) < 0 then
          raise Exception.CreateFmt(
            'Command "%s" references missing parent "%s"',
            [CommandFullPath(Cmd), Cmd.ParentPath]
          );
      end
      else
        RootPaths.Add(Cmd.Name);
    end;
  finally
    RootPaths.Free;
    SeenPaths.Free;
  end;
end;

end.
