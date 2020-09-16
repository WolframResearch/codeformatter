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


## Setup

Install CodeFormatter and dependencies from the paclet server:
```
PacletInstall["CodeParser"]
PacletInstall["CodeFormatter"]
```

CodeFormatter depends on the CodeParser paclet. Make sure that the paclets can be found on your system:
```
Needs["CodeFormatter`"]
```
