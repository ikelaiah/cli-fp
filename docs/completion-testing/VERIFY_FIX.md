# Quick Verification Guide

> **Historical snapshot:** This records completion work and results from
> December 2025. Source line numbers, generated output, file counts, and option
> lists may differ in v1.3.0. See the
> [current Bash guide](BASH_COMPLETION_GUIDE.md) for current behavior.

**Verification Date:** 2025-12-29
**Bug Fix:** Root-level flag completion
**Status:** ✅ VERIFIED - All tests pass!

## Verify the Bug Fix Works

Open Git Bash and run these commands:

```bash
cd /c/Users/iwank/Documents/github/cli-fp/example-bin

# Source the updated completion script
source subcommanddemo_completion.bash

# TEST THE FIX - This was failing before!
./SubCommandDemo.exe --h[TAB][TAB]
```

### Expected Result:
```
--help  --help-complete
```

### If You See This:
✅ **SUCCESS!** The bug is fixed!

I see this output! Yay!

```
iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe --help
--help           --help-complete

iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe --help

```


### If You See Nothing:
❌ Problem - try regenerating the script:
```bash
./SubCommandDemo.exe --completion-file > subcommanddemo_completion.bash
source subcommanddemo_completion.bash
```

---

## Additional Verification Tests

```bash
# Test 1: All root-level long flags
./SubCommandDemo.exe --[TAB][TAB]
# Expected: --help  --help-complete  --version  --completion-file  --completion-file-pwsh

output: Yay!!!

```
iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe --
--completion-file       --completion-file-pwsh  --help                  --help-complete         --version

iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe --

```


# Test 2: All root-level flags (short + long)
./SubCommandDemo.exe -[TAB][TAB]
# Expected: All flags including -h and -v

Output: Yay!!!

```
iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe -
--completion-file       --completion-file-pwsh  --help                  --help-complete         --version               -h                      -v

iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe -
```



# Test 3: Root command completion still works
./SubCommandDemo.exe [TAB]
# Expected: Auto-completes to "repo"

output: Yay!

```
iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe repo

```


# Test 4: Subcommand completion still works
./SubCommandDemo.exe repo [TAB][TAB]
# Expected: --help  -h  clone  init  remote
```

Output: yay

```

iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe repo
--help  -h      clone   init    remote

iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe repo

```
---

## All Tests Pass? Yes!!!!

If all the above tests work as expected, then:

✅ **Root-level flag completion is FIXED**
✅ **Command completion still works**
✅ **Subcommand completion still works**
✅ **Ready for production use!**

You can now move forward with PowerShell testing or any other tasks!
