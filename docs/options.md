# Options and validation

[Start here](getting-started.md) · [How do I...?](how-to.md) ·
[API reference](api-reference.md) · [Limitations](limitations.md)

Register options on the command that uses them. Parsing and validation happen
before `Execute`; retrieve accepted values with `GetParameterValue` inside the
command.

## Register an option

The registration helpers are methods of `TBaseCommand`:

| Need | Registration |
| --- | --- |
| Text | `AddStringParameter('-n', '--name', 'Name', False, 'World')` |
| Integer | `AddIntegerParameter('-c', '--count', 'Count', True)` |
| Float | `AddFloatParameter('-r', '--rate', 'Rate', False, '1.0')` |
| Presence flag | `AddFlag('-v', '--verbose', 'Verbose output')` |
| Explicit Boolean | `AddBooleanParameter('-c', '--color', 'Use colour', False, 'true')` |
| Choice | `AddEnumParameter('-l', '--level', 'Level', 'debug|info|warn', False, 'info')` |
| Path | `AddPathParameter('-p', '--path', 'Target path', True)` |
| URL | `AddUrlParameter('-u', '--url', 'Repository URL', True)` |
| Password | `AddPasswordParameter('-k', '--api-key', 'API key', True)` |
| Date/time | `AddDateTimeParameter('-t', '--time', 'Start time')` |
| Comma-separated items | `AddArrayParameter('-a', '--items', 'Items')` |

`True` in the required position makes an option required. An optional option
with a default returns that default when it was omitted.

## Use a validated value

Values are currently exposed as strings, even after integer, float, Boolean,
or enum validation. Convert them at the command boundary:

```pascal
var
  RawCount: string;
  Count: Integer;
begin
  if GetParameterValue('--count', RawCount) and
     TryStrToInt(RawCount, Count) then
    WriteLn('Count: ', Count);
end;
```

Use `TryStrToFloat` for a float and `SameText(RawValue, 'true')` for a Boolean.
This string-based lookup is a current API boundary, not an indication that
validation was skipped.

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
