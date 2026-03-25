[中文版](README_ZH.md) | **English**

---

# DiffCheck.lsp — AutoCAD Region Diff Comparison Tool

**Compare two drawing regions inside the same DWG and highlight differences with revision clouds.**

---

## What it does

* Select two regions in the same DWG — **Region A (old)** and **Region B (new)**
* Automatically calculates the offset between regions using **Spatial Anchor Voting**
* Compares every object by generating deterministic geometry signatures
* Draws **red revision clouds** around detected differences on Region B
* Nearby changes are **merged into grouped clouds** — clean, readable output
* Oversized background elements (title blocks, borders) are **automatically filtered**
* O(N log N) performance — handles 1400+ objects in seconds

---

## The Problem

AutoCAD's built-in **DWG Compare** only works between two separate files. But architects often keep old and new versions **side by side in the same DWG** — after copying a floor plan to revise it, there's no quick way to spot what actually changed.

```
底圖改了幾條線
→ 肉眼比對兩張圖？
→ 哪裡改了？哪裡沒改？
→ 漏看一條線，施工就出問題
```

Manually scanning two complex drawings for differences is slow, error-prone, and painful.

---

## The Solution

DiffCheck lets you window-select two regions, automatically aligns them, compares every object, and draws revision clouds around the differences — all in seconds.

---

## Installation

1. Download `DiffCheck.lsp`
2. In AutoCAD, type `APPLOAD`
3. Select and load the file
4. Commands `DFC`, `DFCC`, `DFCT` are now available

**Tip:** Add it to AutoCAD's Startup Suite for automatic loading every session.

---

## How to Use

### Step 1 — Run the command

Type `DFC`:

1. **Window-select Region A** (the old / original drawing)
2. **Window-select Region B** (the new / revised drawing)
3. Done — red revision clouds appear around differences on Region B

The command line will show results:

```
Select Region A (old):
  238 objects selected
Select Region B (new):
  238 objects selected
  Auto-align votes: 31
  Generating Signatures & Sorting (Ultra Fast)...
  Matching & Grouping...
  ── Results ──
  Matched (Unchanged): 220
  Changes detected:    18
  All differences marked on Region B (DIFF_CLOUD layer).
  Time: 1.23s
```

### Step 2 — Review

Toggle the `DIFF_CLOUD` layer on/off to review differences against the original drawing.

### Step 3 — Clean up

Type `DFCC` to erase all revision clouds when done.

---

## Commands

| Command | Description |
|---------|-------------|
| `DFC` | Run region comparison |
| `DFCC` | Clear all revision clouds |
| `DFCT` | Adjust merge distance, padding, and arc size |

---

## Supported Entity Types

| Type | Signature Method |
|------|-----------------|
| LINE | Normalized endpoints |
| CIRCLE | Center + radius |
| ARC | Center + radius + angles |
| LWPOLYLINE | Vertices + bulges + closed flag |
| TEXT / MTEXT | Insertion point + height + content |
| INSERT (Block) | Name + insertion point + scale + rotation |
| DIMENSION | Type + measurement value + display text |

---

## How It Works

```
1. Select Region A (old) and Region B (new)

2. Spatial Anchor Offset
   Extract feature points from LINE, CIRCLE, INSERT, TEXT
   → Consensus Voting to find the best displacement vector

3. Signature Generation
   Each entity → deterministic string
   based on type + geometry (rounded to tolerance)

4. Sorted Merge  O(N log N)
   Sort both signature lists
   → single-pass linear scan to find differences

5. Box Merging
   Collect bounding boxes of diff objects
   → merge nearby boxes into groups

6. Draw Revision Clouds
   One red cloud per merged group on DIFF_CLOUD layer
```

---

## Configuration

Adjust with `DFCT` or modify at the top of the file:

| Variable | Default | Description |
|----------|---------|-------------|
| `*dc:tol*` | `2.0` | Coordinate rounding tolerance (drawing units) |
| `*dc:pad*` | `20.0` | Padding around each bounding box |
| `*dc:arc*` | `30.0` | Arc segment length for revision clouds |
| `*dc:merge*` | `50.0` | Max gap for merging nearby difference boxes |
| `*dc:maxbox*` | `0.4` | Giant box filter — ignores objects larger than 40% of region |

**Tips:**
- Too many false positives? Increase `*dc:tol*` (try 5.0 or 10.0)
- Clouds too large and overlapping? Decrease `*dc:merge*`
- Auto-alignment failed? The tool will prompt you to click two matching reference points manually

---

## Notes

| Item | Details |
|------|---------|
| Hatch (fill patterns) | Skipped — seed points are unstable across edits |
| LEADER / MLEADER | Skipped in current version |
| Auto-alignment failed | Tool prompts for manual 2-point alignment |
| 2D only | Z coordinates are not compared |
| Block attributes | INSERT uses name/scale/rotation, not attribute values |

---

## Compatibility

| AutoCAD Version | Status |
|----------------|--------|
| 2014 and above | ✅ Supported |
| Below 2014 | Not tested |

Also works with BricsCAD, GstarCAD, and other AutoLISP-compatible CAD platforms.

---

## Troubleshooting

**Too many clouds / false positives?**
Increase tolerance with `DFCT` or set `*dc:tol*` to 5.0. Dimension text position micro-shifts are the most common cause.

**Offset looks wrong?**
If auto-alignment gets less than 3 votes, the tool will ask you to click two matching reference points. Pick a column center or wall corner that exists in both regions.

**Nothing happened after running?**
Check that both regions contain supported entity types (LINE, CIRCLE, etc.). Objects on locked or frozen layers may not be selected.

---

## Version History

| Version | Notes |
|---------|-------|
| v21 | O(N log N) sorted merge, spatial anchor voting, localized box merging, giant element filter, manual alignment fallback, command aliases changed to DFC/DFCC/DFCT |

---

## Support This Project

If DiffCheck has saved you time, consider buying me a coffee ☕

[![ko-fi](https://ko-fi.com/img/githubbutton_sm.svg)](https://ko-fi.com/beastt1992)

---

## License

MIT License — Free to use, modify, and distribute.

---

**Made with ❤️ for AutoCAD users.**
