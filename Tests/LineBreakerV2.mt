
Needs["CodeFormatter`"]


(*

Was originally formatting as:

f[args___] :=
    $Failed /; (g[])
;

with the ; on a new line

This test is really just that the ; looks good

*)
Test[
	CodeFormat["f[args___] := $Failed /; (g[]);"]
	,
"f[args___] :=
    $Failed /; (g[]);"
	,
	TestID->"LineBreakerV2-20210929-Y8H1A7"
]





(*
was formatting as:

NDSolve`MethodSymbol[method, ndstate["Caller"]] :: tma

*)
Test[
	CodeFormat["NDSolve`MethodSymbol[method, ndstate[\"Caller\"]]::tma"]
	,
	"NDSolve`MethodSymbol[method, ndstate[\"Caller\"]]::tma"
	,
	TestID->"LineBreakerV2-20210929-A6Q1R6"
]









Test[
	CodeFormat["f[1;]"]
	,
"f[1;]"
	,
	TestID->"LineBreakerV2-20211005-M2U2T7"
]



Test[
	CodeFormat["f[1;2;]"]
	,
"f[
    1;
    2;
]"
	,
	TestID->"LineBreakerV2-20211005-B2I2A0"
]





CodeFormat["123 + 456"]

Test[
	CodeFormatter`Private`$LastExtent
	,
	{9, 2, 9, 0}
	,
	TestID->"LineBreakerV2-20211005-X6H3D4"
]


