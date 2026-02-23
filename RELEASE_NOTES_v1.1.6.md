# Release Notes - cli-fp v1.1.6

**Release Date:** February 23, 2026

## 🎉 Overview

Version `1.1.6` adds inline status caption support for progress indicators (spinners and progress bars), making it easier to show live status text like `Scanning...`, `ETA`, or `Processed file 3/10` without manually clearing and rewriting console text.

This release adopts a single caption API for simplicity:

```pascal
Update(const Progress: Integer; const ACaption: string = '')
```

## ✨ New Feature: Progress Indicator Inline Captions

Progress indicators can now display status text inline with the spinner/progress bar.

### Spinner Example

```pascal
Spinner := CreateSpinner(ssLine);
Spinner.Start;
try
  Spinner.Update(0, 'Preparing files...');
finally
  Spinner.Stop;
end;
```

### Progress Bar Example

```pascal
Progress := CreateProgressBar(Count, 20);
Progress.Start;
try
  Progress.Update(i, Format('Processed file %d/%d', [i, Count]));
finally
  Progress.Stop;
end;
```

## 💡 Design Choice (Single API)

The original feature request suggested two possible approaches:

1. A `Caption` property set before each update
2. Passing caption text directly to the update call

Version `1.1.6` intentionally keeps **Approach 2 only** (`Update(..., ACaption)`) because it is:

- Easier to maintain
- More explicit at the call site
- Less prone to stale caption state
- Simpler to test

## 🔄 Improvements

### Cleaner Console Rendering

- Progress output now tracks the previously rendered line length
- Trailing characters are properly cleared when status text becomes shorter
- Prevents visual artifacts when status messages change over time

### Correct Redraw Behavior for Captions

- Progress bars now redraw when the **caption changes**, even if the percentage stays the same
- This is important for workflows where progress advances slowly but status text changes frequently

## ✅ Backward Compatibility

This release is **backward compatible** with existing progress indicator code.

Existing calls such as:

```pascal
Progress.Update(i);
Spinner.Update(0);
```

continue to work unchanged because the caption parameter is optional.

## 🧪 Validation

The updated progress indicator API was validated by compiling all shipped examples:

- `ColorDemo`
- `ErrorHandlingDemo`
- `LongRunningOpDemo`
- `ProgressDemo`
- `SimpleDemo`
- `SubCommandDemo`

`ProgressDemo` was also updated to showcase inline captions for both spinner and progress bar usage.

## 📚 Documentation Updates

The following documentation was updated for `v1.1.6`:

- `README.md`
- `CHANGELOG.md`
- `docs/api-reference.md`
- `docs/technical-docs.md`
- `docs/user-manual.md`
- `examples/ProgressDemo/ProgressDemo.lpr` (example usage)

## 🚀 Upgrade Instructions

No migration steps are required for existing users.

To use inline captions, simply add the optional second parameter when calling `Update`:

```pascal
Progress.Update(CurrentStep, 'Your status text here');
```

## 🙏 Acknowledgments

Thanks to Gus for the feature request and practical feedback on console behavior and status text updates.

## 📝 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

---

**Full Changelog:** [v1.1.5...v1.1.6](https://github.com/ikelaiah/cli-fp/compare/v1.1.5...v1.1.6)
