# Commands and subcommands

[Start here](getting-started.md) · [How do I...?](how-to.md) ·
[Options](options.md) · [Limitations](limitations.md)

Choose the smallest command shape that matches how people invoke the program.

| Shape | Invocation | Use it when |
| --- | --- | --- |
| Root command | `hello --name Ada` | The program has one default action. |
| Named command | `tool greet --name Ada` | The program is a small toolbox. |
| Nested command | `tool repo clone --url …` | Actions have useful groups, like Git. |

## Root command

Pass an unnamed command to the three-argument `CreateCLIApplication` overload:

```pascal
Root := THelloCommand.Create('', 'Print a greeting');
App := CreateCLIApplication('hello', '1.0.0', Root);
```

The root command can have options, and named commands can still be registered
alongside it. Its options only belong to the root action; they are not global
options inherited by named commands.

## Named command

Create a normally named command and register it with the application:

```pascal
Greet := TGreetCommand.Create('greet', 'Print a greeting');
Greet.AddStringParameter('-n', '--name', 'Name to greet', False, 'World');
App := CreateCLIApplication('tool', '1.0.0');
App.RegisterCommand(Greet);
```

This accepts `tool greet --name Ada`. If a command has no root command,
calling `tool` without a named command shows application help instead of
running an action.

## Subcommand

Attach a command to another command before registering the top-level command:

```pascal
Repo := TRepoCommand.Create('repo', 'Repository operations');
Clone := TCloneCommand.Create('clone', 'Clone a repository');
Clone.AddUrlParameter('-u', '--url', 'Repository URL', True);
Repo.AddSubCommand(Clone);
App.RegisterCommand(Repo);
```

This accepts `tool repo clone --url https://example.com/project.git`. A command
that is only a group has subcommands but does not receive options from its
children.

## Help and exit codes

`tool --help` shows application help. `tool greet --help` shows the selected
command. `--version` is an application-level request when used as the first
argument. Return `0` from `Execute` for success and a non-zero integer for an
application failure; `Halt(App.Execute)` forwards it to the shell.

See [How do I return a non-zero exit code?](how-to.md#how-do-i-return-a-non-zero-exit-code)
for the smallest pattern.
