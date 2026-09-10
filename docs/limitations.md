# Current limitations and gotchas

[How do I...?](how-to.md) · [Commands](commands.md) · [Options](options.md)

These are current v1.4.2 boundaries verified against the public source and
tests. They are intentionally easy to find so a limitation does not look like
an undocumented feature.

## Command model

- Positional arguments are not supported.
- Root-command options belong only to the root action. Named commands and
  subcommands do not inherit them.
- The framework has no persistent/global option model across a command tree.
- A command group does not inherit child options.

## Values and validation

- `GetParameterValue` returns strings. Convert validated integers, floats, and
  Booleans in your command code.
- An absent `AddFlag` normally yields the default string `false`; inspect the
  returned value rather than treating the helper's Boolean result as a
  “was supplied” indicator.
- A `Path` parameter validates only that a value was provided. Check existence,
  permissions, and path policy in your command.
- A password is an ordinary string after retrieval. Framework debug output
  redacts registered password values, but your output and external logging must
  redact them too.
- A URL validator accepts only values beginning with `http://`, `https://`,
  `git://`, or `ssh://`.
- Registered integer and float options accept negative values in either
  `--count=-1` or `--count -1` form. For another value beginning with `-`, use
  the equals form so it is not read as a new option.
- Option flags are case-sensitive; use the spelling registered by the command.

## Completion

- Built-in completion covers registered commands, subcommands, options,
  Boolean values, and enum choices.
- Dynamic/custom completion callbacks are deprecated no-op methods in the 1.x
  API. They do not supply filesystem, database, or API candidates.

If one of these boundaries blocks your design, use a small application-level
parser around the framework or open an issue with a concrete desired command
line and expected behavior. Do not rely on a roadmap item as though it were a
released feature.
