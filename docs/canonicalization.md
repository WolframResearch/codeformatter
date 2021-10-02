# canonicalization

strips all whitespace and newlines before formatting: canonicalizes

introduced in CodeFormatter 1.4

fixes hysteresis problem with Airiness



## hysteresis problem

in CodeFormatter 1.3:

If[a,
 b;
 c
 ]

format with Airiness 0.0 first

If[a,
     b;
     c
 ]

format with Airiness -1.0

If[a, b; c]

then format again with Airiness 0.0

If[a,
     b; c
 ]


expected to be original formatting, but it is not




the default "Preserve newline" behavior means that there is a hysteresis

there is not a "canonicalization"

this is because it is "hard" to format WL
when to keep unknown constructs on single line vs. multiple lines?

i originally just said "preserve the given newlines unless otherwise specified"

But this "no canonicalization" is bad for some reasons

1. changing Airiness has a hysteresis

the exact path taken through the Airiness slider matters, this is confusing

Airiness is not "state-based"


2. generated code 

generated code may not have any newlines, so it will look bad if formatted

it will look "different" than hand-written same code




After thinking about the problem for a bit, I have a plan for fixing everything.

need to "canonicalize" first, remove automatic behavior of "Preserve"


remove "Preserve" behavior
for groups: maybe just always do "Delete" as the default behavior?
for CompoundExpressions: maybe just always do "Insert" ?
what are all of the "Preserve" behaviors?





## some exceptions

Not all newlines and whitespace are stripped


It is desired that this comment stay at end of line:

f[
1;
2; (*2a*)
3;
4;
]


This information must be kept track of




























