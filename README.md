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

CodeFormatter and its dependencies are included in Mathematica 12.2 and above.

For older versions, you can install from the paclet server.

The minimum version for CodeFormatter is Mathematica 11.0.

CodeFormatter depends on [CodeParser](https://github.com/WolframResearch/codeparser).

Install CodeFormatter and dependencies from the paclet server:
```
PacletInstall["CodeParser"]
PacletInstall["CodeFormatter"]
```

Make sure that the paclets can be found on your system:
```
Needs["CodeFormatter`"]
```


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
