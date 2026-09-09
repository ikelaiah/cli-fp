# Options and validation

[Start here](getting-started.md) · [How do I...?](how-to.md) ·
[API reference](api-reference.md) · [Limitations](limitations.md)

Options belong to a command object, not to the application globally. Define a
`TBaseCommand` descendant, create its instance during program setup, register
options on that instance, then retrieve accepted values inside that class's
`Execute` method.

## Register an option

This complete **command pattern** defines an option-owning command. The setup
fragment creates its `Cmd` instance; every registration below is placed after
that constructor call and therefore acts on a real `TOptionsCommand` object.

```pascal
uses
  CLI.Command;

type
  TOptionsCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

function TOptionsCommand.Execute: Integer;
begin
  Result := 0;
end;
```

```pascal
var
  Cmd: TOptionsCommand;
begin
  Cmd := TOptionsCommand.Create('configure', 'Configure the application');
  // Register Cmd options here, then register Cmd with the application.
end;
```

In the following table, `Cmd` means that exact `TOptionsCommand` instance.

| Need | Registration on `Cmd` |
| --- | --- |
| Text | `Cmd.AddStringParameter('-n', '--name', 'Name', False, 'World')` |
| Integer | `Cmd.AddIntegerParameter('-c', '--count', 'Count', True)` |
| Float | `Cmd.AddFloatParameter('-r', '--rate', 'Rate', False, '1.0')` |
| Presence flag | `Cmd.AddFlag('-v', '--verbose', 'Verbose output')` |
| Explicit Boolean | `Cmd.AddBooleanParameter('-c', '--color', 'Use colour', False, 'true')` |
| Choice | `Cmd.AddEnumParameter('-l', '--level', 'Level', 'debug|info|warn', False, 'info')` |
| Path | `Cmd.AddPathParameter('-p', '--path', 'Target path', True)` |
| URL | `Cmd.AddUrlParameter('-u', '--url', 'Repository URL', True)` |
| Password | `Cmd.AddPasswordParameter('-k', '--api-key', 'API key', True)` |
| Date/time | `Cmd.AddDateTimeParameter('-t', '--time', 'Start time')` |
| Comma-separated items | `Cmd.AddArrayParameter('-a', '--items', 'Items')` |

`True` in the required position makes an option required. An optional option
with a default returns that default when it was omitted. Complete the program
setup with `App.RegisterCommand(Cmd)`, where `App` is the `ICLIApplication`
variable created with `CreateCLIApplication` as shown in [How-To](how-to.md).

## Use a validated value

Values are currently exposed as strings, even after integer, float, Boolean,
or enum validation. Put conversion in the owning command's `Execute`. This
replacement method for the `TOptionsCommand` pattern above needs `SysUtils`:

```pascal
function TOptionsCommand.Execute: Integer;
var
  RawCount: string;
  Count: Integer;
begin
  if GetParameterValue('--count', RawCount) and
     TryStrToInt(RawCount, Count) then
    WriteLn('Count: ', Count);
  Result := 0;
end;
```

Use `TryStrToFloat` for a float and `SameText(RawValue, 'true')` for an
explicit Boolean. This string-based lookup is a current API boundary, not an
indication that validation was skipped.

## Validation at a glance

- Integer and float values must parse as their respective Pascal types.
- Boolean parameters accept `true` or `false`, case-insensitively. `AddFlag`
  defaults to `false` and becomes `true` when present.
- Enum values match the pipe-separated allowed values case-insensitively.
- URLs must start with `http://`, `https://`, `git://`, or `ssh://`.
- Paths and passwords are accepted as strings; a path is not checked for
  existence and a password is not encrypted.

For syntax rules and surprising boundaries, read [Limitations and gotchas](limitations.md).
For every public signature, use the [API reference](api-reference.md).
