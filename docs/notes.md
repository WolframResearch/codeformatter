# Formatter Notes



use the rewriter to implement a formatter

https://github.com/lshifr/CodeFormatter








blogs:

http://journal.stuffwithstuff.com/2015/09/08/the-hardest-program-ive-ever-written/

https://ambrevar.xyz/indentation/




## FrontEnd

jasonb:
Button in front end "Copy as Formatted Code"






## "Comma First Style"

eslint comma style

https://eslint.org/docs/rules/comma-style.html







## code indentation


Detecting Code Indentation

https://medium.com/firefox-developer-tools/detecting-code-indentation-eff3ed0fb56b




VSCode plugins:

make indentation more readable

https://marketplace.visualstudio.com/items?itemName=oderwat.indent-rainbow


bracket colorizer

https://marketplace.visualstudio.com/items?itemName=CoenraadS.bracket-pair-colorizer-2







## pseudo groups


Switch[a, b, c, d, e]


b, c   and   d, e   are grouped together conceptually



AST should be something like



CallNode[Switch, { test, PseudoGroup[b, c], PseudoGroup[d, e] }, <||>]














# related: automated docs generator







sasha:
* explicit sequences of whitespace characters should be preferred over Tab,
* no trailing whitespaces






#
discussion about pre-receive hooks for stash

https://mail-archive.wolfram.com/archive/t-stash/2018/Oct00/0053.html







things the formatter will do:


move & to same line


























Format TODO






Why do lots of programmers move commas to the next line? [closed]

https://stackoverflow.com/questions/10483635/why-do-lots-of-programmers-move-commas-to-the-next-line







http://ajaxian.com/archives/is-there-something-to-the-crazy-comma-first-style




Haskell Style Guide

https://kowainik.github.io/posts/2019-02-06-style-guide





ESLint

https://eslint.org/docs/rules/comma-style.html






https://scalameta.org/scalafmt/docs/installation.html






#
suggest splitting:

offset = 0;If[endptorder, a, b]




#
insert spaces between ]] and ]

f[a[[x]]]  ->  f[ a[[x]] ]




#
warn about different line endings

\n Unix

\r\n Windows

\r Mac OS 9





















# Design

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







