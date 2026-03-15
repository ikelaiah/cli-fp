unit CliFpGen.Renderer;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, CliFpGen.Model;

function RenderProgramFile(const Spec: TProjectSpec): string;
function RenderRegistryUnit(const Spec: TProjectSpec): string;
function RenderCommandUnit(const Spec: TProjectSpec; const Cmd: TCommandSpec): string;

implementation

uses
  CliFpGen.Naming;

function LinesToText(const Lines: TStrings): string;
begin
  Result := Lines.Text;
end;

function PascalStringLiteral(const S: string): string;
begin
  Result := '''' + StringReplace(S, '''', '''''', [rfReplaceAll]) + '''';
end;

function PascalBoolLiteral(const B: Boolean): string;
begin
  if B then
    Result := 'True'
  else
    Result := 'False';
end;

function PascalWriteLnLiteral(const S: string): string;
begin
  Result := 'WriteLn(' + PascalStringLiteral(S) + ');';
end;

function RenderParameterCall(const VarName: string; const Param: TParameterSpec): string;
begin
  case Param.Kind of
    pkString:
      Result := Format('%s.AddStringParameter(%s, %s, %s, %s, %s);',
        [VarName, PascalStringLiteral(Param.ShortFlag), PascalStringLiteral(Param.LongFlag),
         PascalStringLiteral(Param.Description), PascalBoolLiteral(Param.Required),
         PascalStringLiteral(Param.DefaultValue)]);
    pkInteger:
      Result := Format('%s.AddIntegerParameter(%s, %s, %s, %s, %s);',
        [VarName, PascalStringLiteral(Param.ShortFlag), PascalStringLiteral(Param.LongFlag),
         PascalStringLiteral(Param.Description), PascalBoolLiteral(Param.Required),
         PascalStringLiteral(Param.DefaultValue)]);
    pkFloat:
      Result := Format('%s.AddFloatParameter(%s, %s, %s, %s, %s);',
        [VarName, PascalStringLiteral(Param.ShortFlag), PascalStringLiteral(Param.LongFlag),
         PascalStringLiteral(Param.Description), PascalBoolLiteral(Param.Required),
         PascalStringLiteral(Param.DefaultValue)]);
    pkFlag:
      Result := Format('%s.AddFlag(%s, %s, %s, %s);',
        [VarName, PascalStringLiteral(Param.ShortFlag), PascalStringLiteral(Param.LongFlag),
         PascalStringLiteral(Param.Description), PascalStringLiteral(Param.DefaultValue)]);
    pkBoolean:
      Result := Format('%s.AddBooleanParameter(%s, %s, %s, %s, %s);',
        [VarName, PascalStringLiteral(Param.ShortFlag), PascalStringLiteral(Param.LongFlag),
         PascalStringLiteral(Param.Description), PascalBoolLiteral(Param.Required),
         PascalStringLiteral(Param.DefaultValue)]);
    pkPath:
      Result := Format('%s.AddPathParameter(%s, %s, %s, %s, %s);',
        [VarName, PascalStringLiteral(Param.ShortFlag), PascalStringLiteral(Param.LongFlag),
         PascalStringLiteral(Param.Description), PascalBoolLiteral(Param.Required),
         PascalStringLiteral(Param.DefaultValue)]);
    pkEnum:
      Result := Format('%s.AddEnumParameter(%s, %s, %s, %s, %s, %s);',
        [VarName, PascalStringLiteral(Param.ShortFlag), PascalStringLiteral(Param.LongFlag),
         PascalStringLiteral(Param.Description), PascalStringLiteral(Param.AllowedValues),
         PascalBoolLiteral(Param.Required), PascalStringLiteral(Param.DefaultValue)]);
    pkDateTime:
      Result := Format('%s.AddDateTimeParameter(%s, %s, %s, %s, %s);',
        [VarName, PascalStringLiteral(Param.ShortFlag), PascalStringLiteral(Param.LongFlag),
         PascalStringLiteral(Param.Description), PascalBoolLiteral(Param.Required),
         PascalStringLiteral(Param.DefaultValue)]);
    pkArray:
      Result := Format('%s.AddArrayParameter(%s, %s, %s, %s, %s);',
        [VarName, PascalStringLiteral(Param.ShortFlag), PascalStringLiteral(Param.LongFlag),
         PascalStringLiteral(Param.Description), PascalBoolLiteral(Param.Required),
         PascalStringLiteral(Param.DefaultValue)]);
    pkPassword:
      Result := Format('%s.AddPasswordParameter(%s, %s, %s, %s);',
        [VarName, PascalStringLiteral(Param.ShortFlag), PascalStringLiteral(Param.LongFlag),
         PascalStringLiteral(Param.Description), PascalBoolLiteral(Param.Required)]);
    pkUrl:
      Result := Format('%s.AddUrlParameter(%s, %s, %s, %s, %s);',
        [VarName, PascalStringLiteral(Param.ShortFlag), PascalStringLiteral(Param.LongFlag),
         PascalStringLiteral(Param.Description), PascalBoolLiteral(Param.Required),
         PascalStringLiteral(Param.DefaultValue)]);
  else
    Result := Format('%s.AddStringParameter(%s, %s, %s, %s, %s);',
      [VarName, PascalStringLiteral(Param.ShortFlag), PascalStringLiteral(Param.LongFlag),
       PascalStringLiteral(Param.Description), PascalBoolLiteral(Param.Required),
       PascalStringLiteral(Param.DefaultValue)]);
  end;
end;

function BuildSortedCommands(const Spec: TProjectSpec): TStringList;
var
  i: Integer;
  Path: string;
begin
  Result := TStringList.Create;
  Result.Sorted := True;
  Result.Duplicates := dupError;
  for i := 0 to Spec.Commands.Count - 1 do
  begin
    Path := CommandFullPath(Spec.Commands[i]);
    Result.AddObject(Format('%.4d|%s', [PathDepth(Path), AnsiLowerCase(Path)]), Spec.Commands[i]);
  end;
end;

function RenderProgramFile(const Spec: TProjectSpec): string;
var
  Lines: TStringList;
  ProgramIdent, RegistryUnitName: string;
begin
  ProgramIdent := MakeProgramIdentifier(Spec.AppName);
  RegistryUnitName := MakeRegistryUnitName(Spec.AppName);

  Lines := TStringList.Create;
  try
    Lines.Add('program ' + ProgramIdent + ';');
    Lines.Add('');
    Lines.Add('{$mode objfpc}{$H+}{$J-}');
    Lines.Add('');
    Lines.Add('{ Code generated by cli-fp-gen. DO NOT EDIT. }');
    Lines.Add('{ Put your command implementations in src/commands/*.pas. }');
    Lines.Add('');
    Lines.Add('uses');
    Lines.Add('  SysUtils,');
    Lines.Add('  CLI.Interfaces,');
    Lines.Add('  CLI.Application,');
    Lines.Add('  ' + RegistryUnitName + ';');
    Lines.Add('');
    Lines.Add('var');
    Lines.Add('  App: ICLIApplication;');
    Lines.Add('begin');
    Lines.Add('  try');
    Lines.Add('    App := CreateCLIApplication(' + PascalStringLiteral(Spec.AppName) + ', ' +
      PascalStringLiteral(Spec.AppVersion) + ');');
    Lines.Add('    RegisterGeneratedCommands(App);');
    Lines.Add('    ExitCode := App.Execute;');
    Lines.Add('  except');
    Lines.Add('    on E: Exception do');
    Lines.Add('    begin');
    Lines.Add('      WriteLn(''Error: '' + E.Message);');
    Lines.Add('      ExitCode := 1;');
    Lines.Add('    end;');
    Lines.Add('  end;');
    Lines.Add('end.');
    Result := LinesToText(Lines);
  finally
    Lines.Free;
  end;
end;

function RenderRegistryUnit(const Spec: TProjectSpec): string;
var
  Lines, SortedCmds, UsesUnits: TStringList;
  i: Integer;
  j: Integer;
  Cmd: TCommandSpec;
  UnitName, VarName, ParentVar: string;
  Path: string;
begin
  Lines := TStringList.Create;
  SortedCmds := BuildSortedCommands(Spec);
  UsesUnits := TStringList.Create;
  try
    UsesUnits.Sorted := True;
    UsesUnits.Duplicates := dupIgnore;

    for i := 0 to Spec.Commands.Count - 1 do
      UsesUnits.Add(MakeCommandUnitName(Spec.AppName, CommandFullPath(Spec.Commands[i])));

    Lines.Add('unit ' + MakeRegistryUnitName(Spec.AppName) + ';');
    Lines.Add('');
    Lines.Add('{$mode objfpc}{$H+}{$J-}');
    Lines.Add('');
    Lines.Add('interface');
    Lines.Add('');
    Lines.Add('uses');
    Lines.Add('  CLI.Interfaces;');
    Lines.Add('');
    Lines.Add('{ Code generated by cli-fp-gen. DO NOT EDIT. }');
    Lines.Add('procedure RegisterGeneratedCommands(const App: ICLIApplication);');
    Lines.Add('');
    Lines.Add('implementation');
    Lines.Add('');
    if UsesUnits.Count > 0 then
    begin
      Lines.Add('uses');
      for i := 0 to UsesUnits.Count - 1 do
      begin
        if i < UsesUnits.Count - 1 then
          Lines.Add('  ' + UsesUnits[i] + ',')
        else
          Lines.Add('  ' + UsesUnits[i] + ';');
      end;
      Lines.Add('');
    end;

    Lines.Add('procedure RegisterGeneratedCommands(const App: ICLIApplication);');
    if SortedCmds.Count > 0 then
    begin
      Lines.Add('var');
      for i := 0 to SortedCmds.Count - 1 do
      begin
        Cmd := TCommandSpec(SortedCmds.Objects[i]);
        Path := CommandFullPath(Cmd);
        VarName := MakeCommandVarName(Path);
        Lines.Add('  ' + VarName + ': ' + MakeCommandClassName(Path) + ';');
      end;
    end;
    Lines.Add('begin');
    for i := 0 to SortedCmds.Count - 1 do
    begin
      Cmd := TCommandSpec(SortedCmds.Objects[i]);
      Path := CommandFullPath(Cmd);
      VarName := MakeCommandVarName(Path);
      Lines.Add('  ' + VarName + ' := ' + MakeCommandClassName(Path) + '.Create;');
      Lines.Add('  ' + VarName + '.UpdateDescription(' + PascalStringLiteral(Cmd.Description) + ');');
      for j := 0 to Cmd.Parameters.Count - 1 do
        Lines.Add('  ' + RenderParameterCall(VarName, Cmd.Parameters[j]));
      if Trim(Cmd.ParentPath) = '' then
        Lines.Add('  App.RegisterCommand(' + VarName + ');')
      else
      begin
        ParentVar := MakeCommandVarName(Cmd.ParentPath);
        Lines.Add('  ' + ParentVar + '.AddSubCommand(' + VarName + ');');
      end;
    end;
    Lines.Add('end;');
    Lines.Add('');
    Lines.Add('end.');
    Result := LinesToText(Lines);
  finally
    UsesUnits.Free;
    SortedCmds.Free;
    Lines.Free;
  end;
end;

function RenderCommandUnit(const Spec: TProjectSpec; const Cmd: TCommandSpec): string;
var
  Lines: TStringList;
  FullPath, UnitName, ClassName: string;
begin
  FullPath := CommandFullPath(Cmd);
  UnitName := MakeCommandUnitName(Spec.AppName, FullPath);
  ClassName := MakeCommandClassName(FullPath);

  Lines := TStringList.Create;
  try
    Lines.Add('unit ' + UnitName + ';');
    Lines.Add('');
    Lines.Add('{$mode objfpc}{$H+}{$J-}');
    Lines.Add('');
    Lines.Add('interface');
    Lines.Add('');
    Lines.Add('uses');
    Lines.Add('  CLI.Command;');
    Lines.Add('');
    Lines.Add('{ User-owned stub created by cli-fp-gen. Safe to edit. }');
    Lines.Add('type');
    Lines.Add('  ' + ClassName + ' = class(TBaseCommand)');
    Lines.Add('  public');
    Lines.Add('    constructor Create; reintroduce;');
    Lines.Add('    function Execute: Integer; override;');
    Lines.Add('  end;');
    Lines.Add('');
    Lines.Add('implementation');
    Lines.Add('');
    Lines.Add('constructor ' + ClassName + '.Create;');
    Lines.Add('begin');
    Lines.Add('  inherited Create(' + PascalStringLiteral(Cmd.Name) + ', ' +
      PascalStringLiteral(Cmd.Description) + ');');
    Lines.Add('end;');
    Lines.Add('');
    Lines.Add('function ' + ClassName + '.Execute: Integer;');
    Lines.Add('begin');
    Lines.Add('  if Length(SubCommands) > 0 then');
    Lines.Add('  begin');
    Lines.Add('    ShowHelp;');
    Lines.Add('    Exit(0);');
    Lines.Add('  end;');
    Lines.Add('');
    Lines.Add('  ' + PascalWriteLnLiteral('TODO: Implement command "' + FullPath + '"'));
    Lines.Add('  Result := 0;');
    Lines.Add('end;');
    Lines.Add('');
    Lines.Add('end.');
    Result := LinesToText(Lines);
  finally
    Lines.Free;
  end;
end;

end.
