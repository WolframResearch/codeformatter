# CodeFormatter Notes


## Names

Format
PrettyPrint
http://www.catb.org/~esr/jargon/html/P/prettyprint.html

Beautify
Reformat

Grind
http://www.catb.org/~esr/jargon/html/G/grind.html


```

Needs["Formatter`"]

Needs["Printer`"]

Needs["Prettier`"]

```




## SW notes

SW imagines 3 kind of toggles:
compact:
airy:
very airy: adds \(\* Else \*\) comments

perhaps another dimension:
infixness

perhaps another dimension:
interactive-form vs. non-interactive-form

perhaps another dimension:
mathematical vs. software engineering
mtrott is aware and sent comments

perhaps another dimension:
Write And[a, b] or a && b ?

SW is imagining \~50 options
not all user-visible

How does this interact with spoken code?






## stand-alone tooling

wl-fmt script



## Other formatters for WL

https://github.com/lshifr/CodeFormatter



## Possible names of functions

FormatReplace

TriviaReplace

WhitespaceReplace

AggregateReplace



LanguageFormat

CodeFormat

WLFormat







## FrontEnd ideas

jasonb:
Button in front end "Copy as Formatted Code"






## "Comma First Style"

eslint comma style
https://eslint.org/docs/rules/comma-style.html


Why do lots of programmers move commas to the next line? [closed]
https://stackoverflow.com/questions/10483635/why-do-lots-of-programmers-move-commas-to-the-next-line


http://ajaxian.com/archives/is-there-something-to-the-crazy-comma-first-style








## Code indentation


Detecting Code Indentation

https://medium.com/firefox-developer-tools/detecting-code-indentation-eff3ed0fb56b




VSCode plugins:

make indentation more readable

https://marketplace.visualstudio.com/items?itemName=oderwat.indent-rainbow


bracket colorizer

https://marketplace.visualstudio.com/items?itemName=CoenraadS.bracket-pair-colorizer-2





## Syntax constructs


### Pseudo groups

Switch
others?


Switch[a, b, c, d, e]


b, c   and   d, e   are grouped together conceptually


AST should be something like:

CallNode[Switch, { test, PseudoGroup[b, c], PseudoGroup[d, e] }, <||>]


### Associations and lists of rules

special mode for aligning a -> b of Associations


<|  "a" -> 1,
   "bb" -> 2 |>






## comments from others

sasha:
* explicit sequences of whitespace characters should be preferred over Tab,
* no trailing whitespaces




## Hooks in stash

discussion about pre-receive hooks for stash

https://mail-archive.wolfram.com/archive/t-stash/2018/Oct00/0053.html






## TODO list

things the formatter will do:

move & to same line


suggest splitting:

offset = 0;If[endptorder, a, b]



insert spaces between ]] and ]

f[a[[x]]]  ->  f[ a[[x]] ]



warn about different line endings

\n Unix

\r\n Windows

\r Mac OS 9







## Design

Formatter:

no aggregate changes
no abstract changes
   but still may need to reference abstract syntax:

a[[x] ]

only concrete changes (whitespace, newlines, line continuations)

don't change possible bugs
if something is a possible bug, then after formatting, it should still be a possible bug

example:

{a.
b}

does not remove the newline

example:

{a
b}

does not remove the newline







## Blogs on formatting code

http://journal.stuffwithstuff.com/2015/09/08/the-hardest-program-ive-ever-written/

https://ambrevar.xyz/indentation/


## Links to other languages


Haskell Style Guide
https://kowainik.github.io/posts/2019-02-06-style-guide



ESLint
https://eslint.org/docs/rules/comma-style.html



https://scalameta.org/scalafmt/docs/installation.html







