

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
    $Failed /; (g[]); 
"
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









