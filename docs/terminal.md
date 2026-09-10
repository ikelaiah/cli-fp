# Output, errors, and progress

[How do I...?](how-to.md) · [Examples](examples.md) ·
[API reference](api-reference.md)

Use `CLI.Console` for coloured output and `CLI.Progress` for a spinner or
progress bar. These calls normally live inside the `Execute` method of your
`TBaseCommand` descendant; the framework does not decide what your command
prints.

Colour is disabled when stdout is redirected or when the `NO_COLOR` environment
variable is set. Terminal text removes NUL and ESC control characters while
preserving ordinary prose, Unicode, and line breaks. Coloured writes restore
the previous terminal state even if the underlying output raises an error.

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

`Update` renders immediately; it does not sleep. Choose the refresh cadence in
the work loop that calls it.

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

Progress bars cap their configured visual width at 200 characters, so a bad
configuration cannot request an enormous allocation. `ClearLine` uses ANSI
line clearing on a capable terminal and falls back to a bounded portable
overwrite otherwise.

## Fail clearly

Return a non-zero value from your command's `Execute` for an expected command
failure. Catch unexpected exceptions where your application can add useful
context, then return `1`. The framework reports parse and validation errors
before calling `Execute`.
