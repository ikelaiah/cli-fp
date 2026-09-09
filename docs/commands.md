# Commands and subcommands

[Start here](getting-started.md) · [How do I...?](how-to.md) ·
[Options](options.md) · [Limitations](limitations.md)

In normal v1.4.x code, a command is an object you define by subclassing
`TBaseCommand`. The object owns its registered options; the application owns
the command tree and calls the selected object's `Execute` method.

```text
TBaseCommand
  └── your command class (for example, TGreetCommand)
      ├── Execute
      └── options registered on its instance
your command instance
  └── registered with ICLIApplication
```

Choose the smallest shape that matches how people invoke the program.

| Shape | Invocation | Use it when |
| --- | --- | --- |
| Root command | `hello --name Ada` | The program has one default action. |
| Named command | `tool greet --name Ada` | The program is a small toolbox. |
| Nested command | `tool repo clone --url …` | Actions have useful groups, like Git. |

## Root command

An unnamed descendant is the default action. This complete **command pattern**
defines `TRootCommand`; its empty name is supplied when the instance is made.

```pascal
uses
  CLI.Command;

type
  TRootCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

function TRootCommand.Execute: Integer;
begin
  WriteLn('Running the default action');
  Result := 0;
end;
```

This **program-setup fragment** needs `CLI.Interfaces` and `CLI.Application`.
It creates the `Root` object, registers its option on that object, and passes
the object to the three-argument factory:

```pascal
var
  App: ICLIApplication;
  Root: TRootCommand;
begin
  Root := TRootCommand.Create('', 'Run the default action');
  Root.AddFlag('-v', '--verbose', 'Show detailed output');
  App := CreateCLIApplication('hello', '1.0.0', Root);
  Halt(App.Execute);
end.
```

The root's `Execute` runs for `hello [options]`. Its options only belong to
the root action; they are not global options inherited by named commands.

## Named command

A named descendant is a separately selectable action. This complete
**command pattern** defines the `TGreetCommand` class and its `Execute` method:

```pascal
uses
  CLI.Command;

type
  TGreetCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

function TGreetCommand.Execute: Integer;
begin
  WriteLn('Hello from greet');
  Result := 0;
end;
```

This **program-setup fragment** creates the `Greet` instance, gives that
instance its `--name` option, and registers it with `App`. It needs
`CLI.Interfaces` and `CLI.Application`:

```pascal
var
  App: ICLIApplication;
  Greet: TGreetCommand;
begin
  App := CreateCLIApplication('tool', '1.0.0');
  Greet := TGreetCommand.Create('greet', 'Print a greeting');
  Greet.AddStringParameter('-n', '--name', 'Name to greet', False, 'World');
  App.RegisterCommand(Greet);
  Halt(App.Execute);
end.
```

This accepts `tool greet --name Ada`. With no root command, `tool` by itself
shows application help instead of running an action.

## Subcommand

A nested CLI has a parent command object and a child command object. Both
descend from `TBaseCommand` and must implement `Execute`; the parent can act
as a group even when its method only returns success:

```pascal
uses
  CLI.Command;

type
  TRepoCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

  TCloneCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

function TRepoCommand.Execute: Integer;
begin
  Result := 0;
end;

function TCloneCommand.Execute: Integer;
begin
  WriteLn('Cloning a repository');
  Result := 0;
end;
```

This **program-setup fragment** builds `tool repo clone`. It needs
`CLI.Interfaces` and `CLI.Application`; `Clone`, not `Repo`, owns `--url`:

```pascal
var
  App: ICLIApplication;
  Repo: TRepoCommand;
  Clone: TCloneCommand;
begin
  App := CreateCLIApplication('tool', '1.0.0');
  Repo := TRepoCommand.Create('repo', 'Repository operations');
  Clone := TCloneCommand.Create('clone', 'Clone a repository');
  Clone.AddUrlParameter('-u', '--url', 'Repository URL', True);
  Repo.AddSubCommand(Clone);
  App.RegisterCommand(Repo);
  Halt(App.Execute);
end.
```

This accepts `tool repo clone --url https://example.com/project.git`. A group
does not receive options from its children. For a runnable nested example, see
[SubCommandDemo](https://github.com/ikelaiah/cli-fp/tree/main/examples/SubCommandDemo).

## Help and exit codes

`tool --help` shows application help. `tool greet --help` shows the selected
command. `--version` is an application-level request when used as the first
argument. Return `0` from a command's `Execute` for success and a non-zero
integer for an application failure; the setup fragments use `Halt(App.Execute)`
to forward that result to the shell.

See [How do I return a non-zero exit code?](how-to.md#how-do-i-return-a-non-zero-exit-code)
for a focused method pattern.
