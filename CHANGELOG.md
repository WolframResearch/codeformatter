
## 1.1 - 30 Sep, 2020

Initial public release

### API

Added CodeFormat function

Added Code Formatting palette

Added "Format Cell" button to Package Editor toolbar


## 1.1.1 - 8 Dec, 2020

Included in Mathematica 12.2

### Fixes

The formatter side of what needs to be fixed for 398836: Code Formatting palette turns a b into ab

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
- Fixed a missalignment of the Indentation Menu in the formatter toolbar.
- Reduced the margin at the top of the formatter palette (looks a bit neater now).

Start adding individual style options, and have Airiness resolve to these options

Fix the conflation of level == 0 with being top-level

I was testing level == 0 when formatting CompoundExpressions as a way to prevent newlines from being inserted (CompoundExpressions on a single line at top-level need to stay on a single line)

Introduce a new symbol $Toplevel and control that appropriately

Provide a message for when a cell cannot currently be formatted


## 1.2 - 25 Mar, 2021

Give slightly saner error message when CodeFormat is given bad arguments

Renaming various "Newline" things to "NewlineString" and "CompoundExpressions" things to "Semicolons"

Allow f[ to be formatted in FE

Massive refactoring effort for 12.3

Refactor different "passes" into their own files

Canonicalize what the formatter outputs as not caring about newlines and whitespace that it is given (except in some cases with comments where we care)


### Fixes

Fix formatting a_b ..

Fix bug that resulted in multiple newlines being inserted

Demonstrate bug by doing:

CodeFormat["
f[
1
,
2
]
", "NewlinesBetweenCommas" -> Insert]

and see that there are multiple newlines inserted.

Fix 404196, bad formatting of CompoundExpression in places

Fix 399281, formatter was not handling grouped cells

Fix 406342, line breaking and having:
{
f
[]
}
is weird


## 1.3 - 30 Aug, 2021

Notes on compatibility have been added to docs/compatibility.md

Change default Airiness to be Automatic
