# CodeFormatter

CodeFormatter is a package that provides functionality for formatting Wolfram Language code.

```
Needs["CodeFormatter`"]

CodeFormat["If[a,f/@b,g/@c]"]
```
```
Out[2]= If[a,
            f /@ b
            ,
            g /@ c
        ]

```

[Formatting the Wolfram Language from WTC 2020: Watch Video (youtube)](https://www.youtube.com/watch?v=eGvvKlfaPsQ)


## Setup

CodeFormatter depends on [CodeParser paclet](https://github.com/WolframResearch/codeparser).

CodeFormatter and its dependencies are included in Mathematica 12.2 and above.

For older versions, install CodeFormatter paclet and dependencies from the public paclet server:
```
PacletInstall["CodeParser"]
PacletInstall["CodeFormatter"]
```

[Build and install the CodeFormatter paclet locally](HowToBuild.md)


## Using CodeFormatter

After CodeFormatter is installed, it can be used.

```
Needs["CodeFormatter`"]

CodeFormat["If[a,f/@b,g/@c]"]
```
```
Out[2]= If[a,
            f /@ b
            ,
            g /@ c
        ]

```

The input to `CodeFormat` may be a string, a `File`, or a list of bytes.


## Troubleshooting

Make sure that the paclets can be found on your system:
```
Needs["CodeFormatter`"]
```

and try a basic example:
```
CodeFormat["If[a, b, c]"]
```
