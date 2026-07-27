# Pull Request: Release v1.1.6 - Progress Indicator Inline Status Captions

## 📋 Summary

This PR introduces inline status caption support for progress indicators (spinners and progress bars) in version `1.1.6`, using a single, easy-to-use API:

```pascal
Update(const Progress: Integer; const ACaption: string = '')
```

The implementation keeps existing `Update(Progress)` calls working (via the default parameter), while enabling clean inline status text rendering without manual line clearing.

## 🎯 Type of Change

- [x] New feature (non-breaking change which adds functionality)
- [x] Documentation update
- [x] Improvement/refactor (API simplified to single caption usage style)
- [ ] Bug fix (non-breaking change which fixes an issue)
- [ ] Breaking change (fix or feature that would cause existing functionality to not work as expected)

## ✨ What's New

### 1. Inline Status Captions for Progress Indicators

- Spinners now support inline captions:
  ```pascal
  Spinner.Update(0, 'Scanning files...');
  ```
- Progress bars now support inline captions:
  ```pascal
  Progress.Update(i, Format('Processed file %d/%d', [i, Count]));
  ```
- Caption rendering is handled by the indicator itself (no manual blanking needed)

### 2. Single Supported Caption API (Option 2)

The feature request proposed two options. This release intentionally keeps **Option 2 only**:

- ✅ `Update(Progress, Caption)`
- ❌ `Caption` / `ShowCaption` property API (removed before finalizing)

This keeps the API simpler to maintain and easier to use consistently.

### 3. Smarter Line Rendering

- Progress rendering now tracks previous output length
- Trailing characters are cleared correctly when caption text gets shorter
- Progress bar redraws also occur when **caption changes but percentage does not**

## 📊 Testing

### What Was Verified

- [x] `ProgressDemo` updated to use inline captions for spinner + progress bar
- [x] Progress indicator API compiles with `Update(Progress)` and `Update(Progress, Caption)`
- [x] All example applications compile successfully against the updated API

### Build Verification (FPC 3.2.2)

Compiled successfully:

- `examples/ColorDemo/ColorDemo.lpr`
- `examples/ErrorHandlingDemo/ErrorHandlingDemo.lpr`
- `examples/LongRunningOpDemo/LongRunningOpDemo.lpr`
- `examples/ProgressDemo/ProgressDemo.lpr`
- `examples/SimpleDemo/SimpleDemo.lpr`
- `examples/SubCommandDemo/SubCommandDemo.lpr`

## 🔄 Changes Made

### Source Code

- `src/cli.interfaces.pas`
  - Updated `IProgressIndicator.Update` signature to:
    `Update(const Progress: Integer; const ACaption: string = '')`

- `src/cli.progress.pas`
  - Implemented inline caption rendering for spinner/progress bar
  - Added render-length tracking for clean overwrites
  - Ensured progress bar redraws when caption changes without percentage change

### Examples

- `examples/ProgressDemo/ProgressDemo.lpr`
  - Demonstrates inline captions in both spinner and progress bar phases
  - Removed separate status line output during progress bar processing
  - Bumped demo version string to `1.1.6`

### Documentation

- `CHANGELOG.md` - Added `v1.1.6` entry
- `README.md` - Bumped to `1.1.6` and documented inline caption usage
- `docs/api-reference.md` - Updated progress indicator signatures
- `docs/technical-docs.md` - Updated architecture/API examples
- `docs/user-manual.md` - Added/updated progress examples with inline captions

## 💡 Design Decision

**Why only `Update(Progress, Caption)`?**

- Single API path reduces maintenance cost
- Explicit at call sites (no hidden/stale caption state)
- Easier to test and reason about
- Keeps the feature powerful while staying lightweight

## 📦 Breaking Changes

**None.** Existing calls like `Update(Progress)` remain supported due to the default caption parameter.

## ✅ Checklist

- [x] Code follows the project's style guidelines
- [x] Self-review performed
- [x] Documentation updated (README, CHANGELOG, API docs, technical docs, user manual)
- [x] Example updated (`ProgressDemo`)
- [x] All example projects compile successfully
- [x] No breaking API change for existing `Update(Progress)` usage

## 📚 Related Issues

- Addresses the progress indicator status text/caption feature request (Gus)
- Related branch/topic: `feat/progress-indicators/11-enhance-spinner-and-progress-with-text`

## 🎬 Demo (Example Usage)

```pascal
Spinner := CreateSpinner(ssLine);
Spinner.Start;
try
  Spinner.Update(0, 'Preparing...');
finally
  Spinner.Stop;
end;

Progress := CreateProgressBar(10, 20);
Progress.Start;
try
  Progress.Update(1, 'Processed file 1/10');
finally
  Progress.Stop;
end;
```

## 📖 Documentation

- [Release Notes](RELEASE_NOTES_v1.1.6.md)
- [Changelog](CHANGELOG.md)
- [README](README.md)
- [API Reference](docs/api-reference.md)
- [Technical Docs](docs/technical-docs.md)
- [User Manual](docs/user-manual.md)

## 💬 Additional Notes

This release packages a small but high-impact UX improvement for terminal apps: progress indicators can now display changing status text inline in a clean, framework-managed way.

