BeginPackage["CodeFormatter`Generate`AcceptableOperators`"]

Begin["`Private`"]

Needs["CodeFormatter`Generate`GenerateSources`"]

Print["Generating Acceptable Operators..."]


normalPrefixParselets = Normal[importedPrefixParselets]

normalInfixParselets = Normal[importedInfixParselets]


acceptableOperators =
	
	(*
	Remove prefix operators that are also postfix operators
	*)
	Complement[
		Cases[normalPrefixParselets, Verbatim[Rule][tok_, Parselet`PrefixOperatorParselet[_, _, _]] :> tok],
		Cases[normalInfixParselets, Verbatim[Rule][tok_, Parselet`PostfixOperatorParselet[_, _, _]] :> tok]
	] ~Join~

	Cases[normalPrefixParselets, Verbatim[Rule][tok_, Parselet`GroupParselet[_, _]] :> tok] ~Join~
	Cases[normalInfixParselets, Verbatim[Rule][tok_, Parselet`BinaryOperatorParselet[_, _, _]] :> tok] ~Join~
	Complement[
		Cases[normalInfixParselets, Verbatim[Rule][tok_, Parselet`InfixOperatorParselet[_, _, _]] :> tok],
		(*
		Manually remove Token`Fake`ImplicitTimes
		*)
		{Token`Fake`ImplicitTimes}
	] ~Join~
	Cases[normalInfixParselets, Verbatim[Rule][tok_, Parselet`TildeParselet[]] :> tok] ~Join~
	Cases[normalInfixParselets, Verbatim[Rule][tok_, Parselet`SlashColonParselet[]] :> tok] ~Join~
	Cases[normalInfixParselets, Verbatim[Rule][tok_, Parselet`ColonParselet[]] :> tok] ~Join~
	Cases[normalInfixParselets, Verbatim[Rule][tok_, Parselet`CommaParselet[]] :> tok]



acceptableOperatorsWL = {
"
(*
AUTO GENERATED FILE
DO NOT MODIFY
*)

BeginPackage[\"CodeFormatter`AcceptableOperators`\"]

isAcceptableOperator

Begin[\"`Private`\"]
"} ~Join~

(Row[{"isAcceptableOperator[", #, "]", " ", "=", " ", "True"}]& /@ acceptableOperators) ~Join~

{} ~Join~

{"
isAcceptableOperator[_] = False

End[]

EndPackage[]
"}



Print["exporting AcceptableOperators.wl"]
res = Export[FileNameJoin[{generatedWLDir, "AcceptableOperators.wl"}], Column[acceptableOperatorsWL], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]

Print["Done Acceptable Operators..."]

End[]

EndPackage[]
