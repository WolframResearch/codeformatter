
## Overview of formatter

Using the tree structure from the parser

As context-free as possible

Using a pattern-based approach

Questions like "is this expression too long?" are answered with pattern matching as much as possible


All whitespace is discarded.

Newlines are used to inform decisions.

Categories: just use existing line breaks, always reformat line breaks, some mix of both approaches






## Will only change whitespace and newlines

Nothing to do with adding comments, adding parens, changing to FullForm, etc.

Inserting (*Else*) into If statements is not formatting, it is something else.

Converting And[a, b] into a && b (or other way) now involves output and precedence and parentheses.





## Silently fixes FormatIssues

Issues like this:

```
a/.3->.4
```

are automatically fixed.

It is slightly confusing to report issues like this if the formatter can just fix them automatically.

These sorts of issues "belong" to the formatter, not the linter




