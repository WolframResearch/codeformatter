# CodeFormatter

CodeFormatter is a package that provides functionality for formatting Wolfram Language code.

```
In[1]:= Needs["CodeInspector`"]

In[2]:= CodeFormat["If[a,f/@b,g/@c]"]

Out[2]= If[a,
            f /@ b
            ,
            g /@ c
        ]

```


## Setup

Install CodeFormatter and dependencies from the paclet server:
```
In[1]:= PacletInstall["CodeParser"]
                  PacletInstall["CodeFormatter"]

Out[1]= PacletObject[CodeParser, 1.1, <>]
Out[2]= PacletObject[CodeFormatter, 1.1, <>]
```

CodeFormatter depends on the CodeParser paclet. Make sure that the paclets can be found on your system:
```
In[1]:= Needs["CodeFormatter`"]
```
