# Output, errors, and progress

[How do I...?](how-to.md) · [Examples](examples.md) ·
[API reference](api-reference.md)

Use `CLI.Console` for coloured output and `CLI.Progress` for a spinner or
progress bar. These are normal Pascal calls; the framework does not decide
what your command prints.

## Coloured output

```pascal
uses CLI.Console;

TConsole.WriteLn('Done', ccGreen);
TConsole.WriteLn('Could not connect', ccRed);
```

`TConsole.Write` and `TConsole.WriteLn` also have uncoloured overloads. See
[ColorDemo](https://github.com/ikelaiah/cli-fp/tree/main/examples/ColorDemo)
for a runnable program.

## Spinner

Use a spinner when the duration is unknown. Always stop it in `finally`:

```pascal
var Spinner: IProgressIndicator;
begin
  Spinner := CreateSpinner(ssLine);
  Spinner.Start;
  try
    Spinner.Update(0, 'Downloading');
    Download;
  finally
    Spinner.Stop;
  end;
end;
```

## Progress bar

Use a progress bar when you know the total work:

```pascal
Bar := CreateProgressBar(Count);
Bar.Start;
try
  for Index := 1 to Count do
  begin
    Process(Index);
    Bar.Update(Index, Format('Processed %d of %d', [Index, Count]));
  end;
finally
  Bar.Stop;
end;
```

The [ProgressDemo](https://github.com/ikelaiah/cli-fp/tree/main/examples/ProgressDemo)
contains both patterns.

## Fail clearly

Return a non-zero value from `Execute` for an expected command failure. Catch
unexpected exceptions where your application can add useful context, then
return `1`. The framework reports parse and validation errors before calling
`Execute`.
