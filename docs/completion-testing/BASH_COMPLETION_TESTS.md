# Bash Completion Test Cases for SubCommandDemo

> **Historical snapshot:** This records completion work and results from
> December 2025. Source line numbers, generated output, file counts, and option
> lists may differ in v1.3.0. See the
> [current Bash guide](BASH_COMPLETION_GUIDE.md) for current behavior.

**Test Date:** 2025-12-29
**Tester:** iwank
**Application:** SubCommandDemo.exe
**Shell:** Git Bash (Bash 4.x on Windows)
**Status:** ✅ All tests completed

## Setup Instructions

```bash
# In Git Bash, navigate to the example-bin directory
cd /c/Users/iwank/Documents/github/cli-fp/example-bin

# Source the completion script
source subcommanddemo_completion.bash

# Verify it loaded (should show the completion function)
complete -p SubCommandDemo.exe
```

---

## Test Results

### 1. Root Level Completions

#### Test 1.1: Show root commands
**Command:**
```bash
./SubCommandDemo.exe [TAB][TAB]
```

**Expected:**
```
repo --help --help-complete --version --completion-file --completion-file-pwsh -h
```

**Actual:**
```
iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe repo
--help  -h      clone   init    remote

iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe repo

```

**Status:** Fail

---

#### Test 1.2: Complete 're' prefix
**Command:**
```bash
./SubCommandDemo.exe re[TAB]
```

**Expected:**
```
Completes to: repo
```

**Actual:**
```
iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe repo
```

**Status:** Pass

---

#### Test 1.3: Complete '--h' prefix
**Command:**
```bash
./SubCommandDemo.exe --h[TAB][TAB]
```

**Expected:**
```
--help --help-complete
```

**Actual:**
```
iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe --h
```

**Status:** Fail

---

### 2. Repo Command Level

#### Test 2.1: Show repo subcommands
**Command:**
```bash
./SubCommandDemo.exe repo [TAB][TAB]
```

**Expected:**
```
init clone remote --help -h
```

**Actual:**
```
iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe repo
--help  -h      clone   init    remote

iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe repo

```

**Status:** Pass

---

#### Test 2.2: Complete 'in' prefix
**Command:**
```bash
./SubCommandDemo.exe repo in[TAB]
```

**Expected:**
```
Completes to: init
```

**Actual:**
```
iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe repo init

```

**Status:** Pass

---

#### Test 2.3: Complete 'cl' prefix
**Command:**
```bash
./SubCommandDemo.exe repo cl[TAB]
```

**Expected:**
```
Completes to: clone
```

**Actual:**
```
iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe repo clone

```

**Status:** Pass

---

### 3. Repo Init Command

#### Test 3.1: Show init parameters
**Command:**
```bash
./SubCommandDemo.exe repo init [TAB][TAB]
```

**Expected:**
```
--path -p --bare -b --help -h
```

**Actual:**
```
iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe repo init -
--bare     --help     --path     --version  -b         -h         -p         -v

iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe repo init -

```

**Status:** Pass with extra options shown. Is this right behaviour?

---

#### Test 3.2: Complete '--p' prefix
**Command:**
```bash
./SubCommandDemo.exe repo init --p[TAB]
```

**Expected:**
```
Completes to: --path
```

**Actual:**
```
iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe repo init --path
```

**Status:** Pass

---

#### Test 3.3: Complete '-b' prefix
**Command:**
```bash
./SubCommandDemo.exe repo init -b[TAB]
```

**Expected:**
```
Completes to: --bare or -b
```

**Actual:**
```
iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe repo init -b

```

**Status:** Fail

---

#### Test 3.4: After providing --path
**Command:**
```bash
./SubCommandDemo.exe repo init --path /some/path [TAB][TAB]
```

**Expected:**
```
--bare -b --help -h (remaining options)
```

**Actual:**
```
iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe repo init --path /some/path -
--bare     --help     --path     --version  -b         -h         -p         -v

iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe repo init --path /some/path -

```

**Status:** Pass with extra options shown

---

### 4. Repo Clone Command

#### Test 4.1: Show clone parameters
**Command:**
```bash
./SubCommandDemo.exe repo clone [TAB][TAB]
```

**Expected:**
```
--url -u --path -p --branch -b --depth -d --help -h
```

**Actual:**
```
$ ./SubCommandDemo.exe repo clone -
--branch   --depth    --help     --path     --url      --version  -b         -d         -h         -p         -u         -v

iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe repo clone -
```

**Status:** Pass with extras options shown

---

#### Test 4.2: Complete '--u' prefix
**Command:**
```bash
./SubCommandDemo.exe repo clone --u[TAB]
```

**Expected:**
```
Completes to: --url
```

**Actual:**
```
iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe repo clone --url


```

**Status:** Pass

---

#### Test 4.3: Complete '--b' prefix
**Command:**
```bash
./SubCommandDemo.exe repo clone --b[TAB]
```

**Expected:**
```
Completes to: --branch
```

**Actual:**
```
iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe repo clone --branch

```

**Status:** Pass

---

#### Test 4.4: After providing --url
**Command:**
```bash
./SubCommandDemo.exe repo clone --url https://example.com [TAB][TAB]
```

**Expected:**
```
--path -p --branch -b --depth -d --help -h
```

**Actual:**
```
iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe repo clone --url https://example.com

```

**Status:** Fail

---

### 5. Repo Remote Command (Command Group)

#### Test 5.1: Show remote subcommands
**Command:**
```bash
./SubCommandDemo.exe repo remote [TAB][TAB]
```

**Expected:**
```
add remove --help -h
```

**Actual:**
```
iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe repo remote
--help  -h      add     remove

iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe repo remote

```

**Status:** Pass

---

#### Test 5.2: Complete 'a' prefix
**Command:**
```bash
./SubCommandDemo.exe repo remote a[TAB]
```

**Expected:**
```
Completes to: add
```

**Actual:**
```
iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe repo remote add

```

**Status:** Pass

---

#### Test 5.3: Complete 'rem' prefix
**Command:**
```bash
./SubCommandDemo.exe repo remote rem[TAB]
```

**Expected:**
```
Completes to: remove
```

**Actual:**
```
iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe repo remote remove

```

**Status:** Pass

---

### 6. Repo Remote Add Command

#### Test 6.1: Show remote add parameters
**Command:**
```bash
./SubCommandDemo.exe repo remote add [TAB][TAB]
```

**Expected:**
```
--name -n --url -u --help -h
```

**Actual:**
```

iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe repo remote add -
--help     --name     --url      --version  -h         -n         -u         -v

iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe repo remote add -

```

**Status:** Pass with extra options shown

---

#### Test 6.2: Complete '--n' prefix
**Command:**
```bash
./SubCommandDemo.exe repo remote add --n[TAB]
```

**Expected:**
```
Completes to: --name
```

**Actual:**
```
iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe repo remote add --name

```

**Status:** Pass

---

#### Test 6.3: After providing --name
**Command:**
```bash
./SubCommandDemo.exe repo remote add --name origin [TAB][TAB]
```

**Expected:**
```
--url -u --help -h
```

**Actual:**
```
iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe repo remote add --name origin -
--help     --name     --url      --version  -h         -n         -u         -v

iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe repo remote add --name origin -

```

**Status:** Pass with extra options shown

---

### 7. Repo Remote Remove Command

#### Test 7.1: Show remote remove parameters
**Command:**
```bash
./SubCommandDemo.exe repo remote remove [TAB][TAB]
```

**Expected:**
```
--name -n --help -h
```

**Actual:**
```
iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe repo remote remove -
--help     --name     --version  -h         -n         -v

iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe repo remote remove -

```

**Status:** Pass

---

#### Test 7.2: Complete '--n' prefix
**Command:**
```bash
./SubCommandDemo.exe repo remote remove --n[TAB]
```

**Expected:**
```
Completes to: --name
```

**Actual:**
```
iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe repo remote remove --name

```

**Status:** Pass

---

### 8. Short vs Long Flags

#### Test 8.1: Short flag completion
**Command:**
```bash
./SubCommandDemo.exe repo init -[TAB][TAB]
```

**Expected:**
```
-p -b -h
```

**Actual:**
```
iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe repo init -
--bare     --help     --path     --version  -b         -h         -p         -v

iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe repo init -

```

**Status:** Pass with extra options shown

---

#### Test 8.2: Long flag completion
**Command:**
```bash
./SubCommandDemo.exe repo init --[TAB][TAB]
```

**Expected:**
```
--path --bare --help
```

**Actual:**
```
iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe repo init --
--bare     --help     --path     --version

iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe repo init --

```

**Status:** Pass with extra options shown

---

### 9. Mixed Parameter Order

#### Test 9.1: Parameters in different order
**Command:**
```bash
./SubCommandDemo.exe repo clone --branch main [TAB][TAB]
```

**Expected:**
```
--url -u --path -p --depth -d --help -h
```

**Actual:**
```
[Fill in actual output here]
```

**Status:** ☐ Pass ☐ Fail

---

#### Test 9.2: Multiple parameters provided
**Command:**
```bash
./SubCommandDemo.exe repo clone --url https://test.com --branch main [TAB][TAB]
```

**Expected:**
```
--path -p --depth -d --help -h
```

**Actual:**
```
iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe repo clone --url https://test.com --branch main

```

**Status:** Fail, no further options shown

---

### 10. Edge Cases

#### Test 10.1: Invalid command path
**Command:**
```bash
./SubCommandDemo.exe repo invalid [TAB][TAB]
```

**Expected:**
```
No completions or only --help -h
```

**Actual:**
```
iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe repo invalid
```

**Status:** Pass kinda, no completions shown. Not even -h or --help

---

#### Test 10.2: Deep nesting works
**Command:**
```bash
./SubCommandDemo.exe repo remote add --name test --url [TAB]
```

**Expected:**
```
Should wait for URL input (no completions for URL values)
```

**Actual:**
```
iwank@DESKTOP-JQA83SU MINGW64 ~/Documents/github/cli-fp/example-bin (feature/completion-file-for-bash-ps)
$ ./SubCommandDemo.exe repo remote add --name test --url

```

**Status:** Pass

---

## Summary

**Total Tests:** 30
**Passed:** [Fill in]
**Failed:** [Fill in]

### Failed Tests Details
```
[List any failed tests and their issues here]
```

### Notes
```
[Add any additional observations or notes here]
```
