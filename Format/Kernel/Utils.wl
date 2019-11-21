BeginPackage["Format`Utils`"]

SourceTake


shadows


Begin["`Private`"]

Needs["Format`"]
Needs["AST`"]
Needs["AST`Utils`"]


(*
input:
	str: the contents
	{{line1_, col1_}, {line2_, col2_}}: the Source spec to take from str
*)
SourceTake[str_String, {{line1_, col1_}, {line2_, col2_}}] :=
Module[{lines},
	lines = StringSplit[str, "\n"];
	If[line1 == line2,
		StringTake[lines[[line1]], col1 ;; col2]
		,
		StringJoin[Riffle[{
			StringTake[lines[[line1]], col1 ;;],
			Sequence @@ lines[[line1 + 1 ;; line2 - 1]],
			StringTake[lines[[line2]], ;; col2]}, "\n"]]
	]
]







(*
shadows[action1, action2] => True means action1 is less severe and is contained by action2; we will get rid of issue1
*)
shadows[action1:CodeTextAction[_, command1_, data1_], action2:CodeTextAction[_, command2_, data2_]] :=
	Which[
		action1 === action2,
			False,
		command1 =!= command2,
			False,
		SourceMemberQ[data2[ Source ], data1[ Source ] ],
			True,
		True,
			False
	]

shadows[args___] := Failure["InternalUnhandled", <|"Function" -> shadows, "Arguments" -> HoldForm[{args}]|>]









End[]

EndPackage[]
