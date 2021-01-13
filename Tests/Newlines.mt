
Needs["CodeFormatter`"]

Test[
	CodeFormat["
f[
1
,
2
]
", "NewlinesBetweenCommas" -> Insert]
	,
"f[
    1
    ,
    2
]"
	,
	TestID->"Newlines-20201211-N3B5I5"
]


Test[
	CodeFormat["( f /@ {} )", "NewlinesBetweenOperators" -> Insert]
	,
"(f
    /@
    {})"
	,
	TestID->"Newlines-20201211-C2J3H1"
]




Test[
	CodeFormat["f[If[a, b[]]; g[]]"]
	,
"f[If[a,
    b[]
];
g[]]"
	,
	TestID->"Newlines-20210113-O4V3Q0"
]




