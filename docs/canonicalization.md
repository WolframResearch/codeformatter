# canonicalization

effort to canonicalize output of CodeFormatter



fixes hysteresis problem with Airiness




largely done by simply removing newlines and whitespace before formatting.




But there are some places where newlines cannot be removed




## Comments at start/end of lines


how to solve this problem?

f[
1;
2; (*2a*)
3;
4;
]





first remove all whitespace and newlines:

f[1;2;(*2a*)3;4;]




How to distinguish between:


f[
1;
2; (*2a*)
3;
4;
]


and





f[
1;
2;
(*2a*)
3;
4;
]






also comments like this:

state = runFun[
    state, proposals, lrun, {bdUniforms, ratioUniforms},
    {ratioCommonFactor, p (*must be machprec*)}, args, machPrecQ
];














f[
1;
2; (*2a*)
3;
4;
]

strips as:

f[1;2;(*2a*)
3;4;]






f[
1;
2;
(*2a*)
3;
4;
]

strips as:

f[1;2;
(*2a*)
3;4;]







at start of line, comment "absorbs" the leading newline

at end of line, comment "absorbs" the trailing newline



















