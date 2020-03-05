# CodeFormatter

CodeFormatter is a package that provides functionality for formatting Wolfram Language code.


## Setup

CodeFormatter depends on the CodeParser paclet. Make sure that the paclets can be found on your system:
```
In[1]:= Needs["CodeParser`"]
      Needs["CodeFormatter`"]
```

[CodeParser on github.com](https://github.com/<<TODO_placeholder_for_actual_link>>)
[CodeFormatter on github.com](https://github.com/<<TODO_placeholder_for_actual_link>>)

Install CodeFormatter and dependencies from the CodeTools paclet server:
```
In[1]:= PacletUpdate["CodeParser", "Site" -> "<<TODO_placeholder_for_actual_link>>", "UpdateSites" -> True]
      PacletUpdate["CodeFormatter", "Site" -> "<<TODO_placeholder_for_actual_link>>", "UpdateSites" -> True]

Out[1]= PacletObject[CodeParser, 1.0, <>]
Out[2]= PacletObject[CodeFormatter, 1.0, <>]
```


## Building

CodeFormatter uses a Wolfram Language kernel to build a `.paclet` file.

CodeFormatter uses CMake to generate build scripts.

Here is an example transcript using the default make generator to build CodeFormatter:
```
cd codeformatter
mkdir build
cd build
cmake ..
cmake --build . --target paclet
```

The result is a directory named `paclet` that contains the WL package source code and a built CodeFormatter `.paclet` file for installing.

Specify `MATHEMATICA_INSTALL_DIR` if you have Mathematica installed in a non-default location:
```
cmake -DMATHEMATICA_INSTALL_DIR=/Applications/Mathematica111.app/Contents/ ..
cmake --build . --target paclet
```
