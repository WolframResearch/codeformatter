BeginPackage["Format`Utils`"]

SourceTake

Begin["`Private`"]

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

End[]

EndPackage[]
