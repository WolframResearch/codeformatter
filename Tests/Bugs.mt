(* Wolfram Language Test file *)

Needs["CodeFormatter`"]

Needs["CodeParser`"]



(*
bug 398836
*)
cst = CodeConcreteParseBox[RowBox[{"a", " ", "b"}]]

Test[
	CodeFormatCST[cst]
	,
"a b
"
	,
	TestID->"Bugs-20201016-T9R8A5"
]





cst = CodeConcreteParseBox[RowBox[{"a", " ", "b"}]]

Test[
	CodeFormat["1 + 2222222222222222222222222 + 3", "LineWidth" -> 20]
	,
"1 + 2222222222222222222222222 +
     3"
	,
	TestID->"Bugs-20201016-J1C9L9"
]



