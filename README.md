[中文版](README_ZH.md) | **English**

---

# DiffCheck.lsp — Auto-Mark Revision Clouds for Design Changes

**Stop circling changes by hand. Select old and new, get revision clouds instantly.**

---

## What it does

Every design revision requires marking what changed with revision clouds — for review, for building permits, for your own sanity. Everyone does this by hand. DiffCheck does it in seconds.

* Select **Region A (old)** and **Region B (new)** in the same DWG
* Automatically aligns the two regions using **Spatial Anchor Voting**
* Compares every object and finds what's different
* Draws **red revision clouds** around changes — ready for submission
* Nearby changes are **merged into clean grouped clouds**
* O(N log N) performance — 1400+ objects in seconds

---

## The Problem

```
Design revision submitted
→ Circle all changes with revision clouds
→ Did I get them all? Did I miss that wall I moved?
→ Reviewer finds unmarked changes
→ Rejected, redo
```

Manual cloud marking is tedious, error-prone, and happens every single revision cycle. AutoCAD's built-in DWG Compare only works between two separate files and outputs color overlays — not revision clouds you can submit.

---

## The Solution

DiffCheck compares two regions inside the same DWG and outputs real revision clouds on a dedicated layer. Toggle the layer, print, submit.

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

1. **Window-select Region A** — the previous version
2. **Window-select Region B** — the revised version
3. Done — red revision clouds appear around every change

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

### Step 2 — Review and submit

The clouds are on the `DIFF_CLOUD` layer. Toggle visibility, adjust if needed, print.

### Step 3 — Clean up

Type `DFCC` to erase all revision clouds when done.

---

## Commands

| Command | Description |
|---------|-------------|
| `DFC` | Run comparison, generate revision clouds |
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
| INSERT (Block) | Name + insertion point + scale + rotation + **attribute tags & values** |
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
   Blocks include attribute tag=value pairs

4. Sorted Merge  O(N log N)
   Sort both lists → single-pass linear scan

5. Box Merging
   Nearby diff bounding boxes → merged groups

6. Revision Clouds
   One cloud per group on DIFF_CLOUD layer
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
- Clouds too large? Decrease `*dc:merge*`
- Auto-alignment failed? The tool prompts you to click two matching reference points manually

---

## Notes

| Item | Details |
|------|---------|
| Hatch (fill patterns) | Skipped — seed points are unstable across edits |
| LEADER / MLEADER | Skipped in current version |
| Auto-alignment failed | Prompts for manual 2-point alignment |
| 2D only | Z coordinates are not compared |
| Block attributes | ✅ Compared — tag names and values are included in signature |

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
Increase tolerance with `DFCT` or set `*dc:tol*` to 5.0. Dimension text micro-shifts are the most common cause.

**Offset looks wrong?**
If auto-alignment gets less than 3 votes, you'll be prompted to click two matching reference points. Pick a column center or wall corner that exists in both regions.

**Nothing happened after running?**
Check that both regions contain supported entity types. Objects on locked or frozen layers may not be selected.

---

## Version History

| Version | Notes |
|---------|-------|
| v21.1 | Added block attribute deep comparison (tag names & values) |
| v21 | O(N log N) sorted merge, spatial anchor voting, localized box merging, giant element filter, manual alignment fallback |

---

## Support This Project

If DiffCheck saved you from hand-circling revision clouds, consider buying me a coffee ☕

[![ko-fi](https://ko-fi.com/img/githubbutton_sm.svg)](https://ko-fi.com/beastt1992)

---

## License

MIT License — Free to use, modify, and distribute.

---

**Made with ❤️ for architects who are tired of circling clouds by hand.**
