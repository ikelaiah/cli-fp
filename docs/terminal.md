# Output, errors, and progress

[How do I...?](how-to.md) · [Examples](examples.md) ·
[API reference](api-reference.md)

Use `CLI.Console` for coloured output and `CLI.Progress` for a spinner or
progress bar. These calls normally live inside the `Execute` method of your
`TBaseCommand` descendant; the framework does not decide what your command
prints.

## Coloured output

Import `CLI.Console` in the command unit and call the `TConsole` class
directly—there is no console instance to declare. This is an **`Execute`-body
fragment**:

```pascal
uses
  CLI.Console;

TConsole.WriteLn('Done', ccGreen);
TConsole.WriteLn('Could not connect', ccRed);
```

`TConsole.Write` and `TConsole.WriteLn` also have uncoloured overloads. See
[ColorDemo](https://github.com/ikelaiah/cli-fp/tree/main/examples/ColorDemo)
for a runnable program.

## Spinner

Use a spinner when the duration is unknown. This complete **`Execute`-method
fragment** belongs to a declared `TDownloadCommand = class(TBaseCommand)`;
its command unit needs `CLI.Interfaces` and `CLI.Progress`:

```pascal
function TDownloadCommand.Execute: Integer;
var
  Spinner: IProgressIndicator;
begin
  Spinner := CreateSpinner(ssLine);
  Spinner.Start;
  try
    Spinner.Update(0, 'Downloading');
    // Perform the download here.
  finally
    Spinner.Stop;
  end;
  Result := 0;
end;
```

Always stop the indicator in `finally`.

## Progress bar

Use a progress bar when you know the total work. This complete **`Execute`-
method fragment** belongs to a declared `TBatchCommand = class(TBaseCommand)`;
its command unit needs `SysUtils`, `CLI.Interfaces`, and `CLI.Progress`:

```pascal
function TBatchCommand.Execute: Integer;
var
  Bar: IProgressIndicator;
  Index: Integer;
  Total: Integer;
begin
  Total := 3;
  Bar := CreateProgressBar(Total);
  Bar.Start;
  try
    for Index := 1 to Total do
    begin
      // Process item Index here.
      Bar.Update(Index, Format('Processed %d of %d', [Index, Total]));
    end;
  finally
    Bar.Stop;
  end;
  Result := 0;
end;
```

The [ProgressDemo](https://github.com/ikelaiah/cli-fp/tree/main/examples/ProgressDemo)
contains both patterns.

## Fail clearly

Return a non-zero value from your command's `Execute` for an expected command
failure. Catch unexpected exceptions where your application can add useful
context, then return `1`. The framework reports parse and validation errors
before calling `Execute`.
