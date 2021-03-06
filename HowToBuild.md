# Building

CodeFormatter uses a Wolfram Language kernel to build a `.paclet` file.

CodeFormatter uses CMake to generate build scripts.

Here is an example transcript using the default make generator to build CodeFormatter:

```
cd codeformatter
mkdir build
cd build
cmake ..
cmake --build .
```

The result is a directory named `paclet` that contains the WL package source code and a built CodeFormatter `.paclet` file for installing.

Inside a kernel session you may then install the paclet by evaluating:
```
PacletInstall["/path/to/build/paclet/CodeFormatter-1.3.paclet"]
```

Specify `MATHEMATICA_INSTALL_DIR` if you have Mathematica installed in a non-default location:

```
cmake -DMATHEMATICA_INSTALL_DIR=/Applications/Mathematica123.app/Contents/ ..
cmake --build .
```

On Windows:

```
cmake -DMATHEMATICA_INSTALL_DIR="C:/Program Files/Wolfram Research/Mathematica/12.3" ..
cmake --build .
```

## Installing

You can install the paclet from CMake:
```
cmake --install .
```

This starts a kernel and calls `PacletInstall` with the built .paclet file.
