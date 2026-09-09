# Contributing and support

`cli-fp` is a small open-source framework maintained around a deliberately
small public API. v1.4.1 is a documentation-accuracy patch: the supported
runtime is still the class-based API described in these guides.

## Supported environments

- Free Pascal 3.2.2 is the tested compiler version.
- Windows and Linux run the repository's CI checks.
- macOS, FreeBSD and other FPC-supported Unix systems are expected to work but
  are not currently exercised in CI.
- The runtime framework has no third-party dependencies. Lazarus is optional;
  the repository includes a [runtime package](https://github.com/ikelaiah/cli-fp/blob/main/packages/lazarus/cli_fp.lpk).

## Contribute a change

Read the [contribution guide](https://github.com/ikelaiah/cli-fp/blob/main/CONTRIBUTING.md),
then run the platform-appropriate framework, generator and cleanup checks
before opening a pull request. Documentation changes should keep examples
runnable and should not expose internal units as beginner APIs.

Useful project records:

- [Roadmap](https://github.com/ikelaiah/cli-fp/blob/main/ROADMAP.md) — planned
  runtime work and explicit non-goals.
- [Changelog](https://github.com/ikelaiah/cli-fp/blob/main/CHANGELOG.md) —
  release history.
- [Issues](https://github.com/ikelaiah/cli-fp/issues) — bug reports and
  focused enhancement discussions.

When documentation and source disagree, treat the current public units under
[`src/`](https://github.com/ikelaiah/cli-fp/tree/main/src) and behaviour covered
by tests as authoritative.
