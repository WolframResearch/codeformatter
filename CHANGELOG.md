
## 1.4 - 25 Oct, 2021

Do not allow PacletManager to participate in finding \`Generate\` files


Palette Feature 1: Airiness Slider or Newline Togglers

The airiness of code formatting is controlled via the familiar slider
control or a new set of toggler controls. Both update the same
underlying options. The slider can be considered a coarse-grained control
while the togglers individually target various newline formatting
options. These fine-grained options include visualizations and tooltips
to describe the formatting behavior that they modify.

Palette Feature 2: Presets

The state of the formatting options can be saved between desktop
sessions and recalled at a later time. Multiple formatting presets can
be saved. The most recently selected preset is remembered automatically
between desktop sessions. Presets can be updated to match the current
state of the formatting options. The indentation style is included as
part of the preset.

Naming and editing the presets is done via subdialogs because editable
InputFields are not allowed in floating palettes.

Implementation Details:

* some formatting controls exist in the Package and Script editors. The
highlight colors are controlled from "CodeFormatterHighlightColor"
styles defined in the paclet's Package.nb and Script.nb stylesheets
* speech bubble popups are modeled after those from Drawing Tools
* the palette state is saved between sessions via key-values stored in
CurrentValue[$FrontEnd, {CodeAssistOptions, "CodeToolsOptions",
"CodeFormat"}]


Abstract the outer `[]` and inner `[]` groups into a single `[[]]` group

Ensure that `]]` are not broken


Canonicalize input before indenting

Rely on graphical syntax; no more reliance on whitespace / newlines that were passed in

2 stages: top-down indenting, then symbolic rendering


If `;` is last in CompoundExpression, then rename to something special

Rename Token\`Semi -> Token\`Fake\`SemiBeforeImplicitNull

Allow for easier processing later


Add LineBreakerV2 method, off by default


### Fixes

Related to 413985: CodeFormat does not handle empty list of bytes

Return unevaluated for now


Explicitly handle `CodeFormat[{}]`

Fix 414042: `&` on newline giving linter warnings

Fix unreported bug: malformed Whitespace could be generated in certain edge cases

Fix problems with merging temporary line continuations

Fix 415177: ``"Get::noopen: Cannot open Forms`." `` message when building

Fix 415178: spurious DynamicImageSize warning

Handle selecting Section cells and pressing `"Format Cell"`

Add a menu position for the CodeFormatter palette so that the Code related palettes appear in their own section of the Palettes menu.


## 1.3 - 30 Aug, 2021

Notes on compatibility have been added to docs/compatibility.md

Change default Airiness to be Automatic


## 1.2 - 25 Mar, 2021

Give slightly saner error message when CodeFormat is given bad arguments

Renaming various `"Newline"` things to `"NewlineString"` and `"CompoundExpressions"` things to `"Semicolons"`

Allow `f[` to be formatted in FE

Massive refactoring effort for 12.3

Refactor different "passes" into their own files

Canonicalize what the formatter outputs as not caring about newlines and whitespace that it is given (except in some cases with comments where we care)


### Fixes

Fix formatting `a_b ..`

Fix bug that resulted in multiple newlines being inserted

Demonstrate bug by doing:
```
CodeFormat["
f[
1
,
2
]
", "NewlinesBetweenCommas" -> Insert]
```

and see that there are multiple newlines inserted.

Fix 404196, bad formatting of CompoundExpression in places

Fix 399281, formatter was not handling grouped cells

Fix 406342, line breaking and having:
```
{
f
[]
}
```
is weird


## 1.1.1 - 8 Dec, 2020

Included in Mathematica 12.2

### Fixes

The formatter side of what needs to be fixed for 398836: Code Formatting palette turns `a b` into `ab`

Remove excess implicit Times tokens where needed.

Also make sure to do sanity checking on formatting cells. This will prevent any bugs in the formatter from propagating errors into user source code.

Fix CodeFormat mis-formatting last comma in Switch

Convert string literals in palette to use FrontEndResource

Add text resources to be used by FE

Make sure to only use the FE text resources as labels for the radio button bar

The actual values should still be "tab" and "space"

Translation of CodeFormatting palette's strings

Adding simplified Chinese strings

Redesign CodeFormatter settings DockedCell and palette

Handle integer Airiness values of -1, 0, 1

- Increased the width of the Indentation Menu to fit the Spanish resource `"TabMenuItem" -> "Tabulaciones"` in CodeFormatter.tr
- Fixed a misalignment of the Indentation Menu in the formatter toolbar.
- Reduced the margin at the top of the formatter palette (looks a bit neater now).

Start adding individual style options, and have Airiness resolve to these options

Fix the conflation of level == 0 with being top-level

I was testing level == 0 when formatting CompoundExpressions as a way to prevent newlines from being inserted (CompoundExpressions on a single line at top-level need to stay on a single line)

Introduce a new symbol $Toplevel and control that appropriately

Provide a message for when a cell cannot currently be formatted


## 1.1 - 30 Sep, 2020

Initial public release

### API

Added CodeFormat function

Added Code Formatting palette

Added `"Format Cell"` button to Package Editor toolbar
