
## Overview of formatter

Using the tree structure from the parser

As context-free as possible

Using a pattern-based approach

Questions like "is this expression too long?" are answered with pattern matching as much as possible


All whitespace and newlines are discarded[\*].

\* except when there is a canonicalization issue






## Will only change whitespace and newlines

Nothing to do with adding comments, adding parens, changing to FullForm, etc.

Inserting (*Else*) into If statements is not formatting, it is something else.

Converting And[a, b] into a && b (or vice versa) now involves output and precedence and parentheses.

Using CodeFormatter to insert \[LeftDoubleBracket] \[RightDoubleBracket] characters or \[Rule] character is not formatting, it is something else.

Breaking up CompoundExpression[] at top-level is not formatting, it is something else, changes semantics.


Formatting is a function concrete -> concrete
PrettyPrinting is a function expr -> concrete



## Silently fixes FormatIssues

Issues like this:

```
a/.3->.4
```

are automatically fixed.

It is slightly confusing to report issues like this if the formatter can just fix them automatically.

These sorts of issues "belong" to the formatter, not the linter




