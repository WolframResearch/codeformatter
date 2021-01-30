
If[!MemberQ[$Path, #], PrependTo[$Path, #]]&[DirectoryName[$InputFileName, 3]]

BeginPackage["CodeFormatter`Generate`AcceptableOperators`"]

Begin["`Private`"]

Needs["CodeTools`Generate`GenerateSources`"]


dataDir = FileNameJoin[{srcDir, "CodeParser", "Data"}]


importedPrefixParselets = Get[FileNameJoin[{dataDir, "PrefixParselets.wl"}]]

importedInfixParselets = Get[FileNameJoin[{dataDir, "InfixParselets.wl"}]]


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

		Manually remove Token`Dot, because we do not want to break after  a =.
		*)
		{Token`Fake`ImplicitTimes, Token`Dot}
	] ~Join~
	Cases[normalInfixParselets, Verbatim[Rule][tok_, Parselet`TildeParselet[]] :> tok] ~Join~
	Cases[normalInfixParselets, Verbatim[Rule][tok_, Parselet`SlashColonParselet[]] :> tok] ~Join~
	Cases[normalInfixParselets, Verbatim[Rule][tok_, Parselet`ColonParselet[]] :> tok] ~Join~
	Cases[normalInfixParselets, Verbatim[Rule][tok_, Parselet`CommaParselet[]] :> tok]


generate[] := (

Print["Generating Acceptable Operators..."];

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
"};

Print["exporting AcceptableOperators.wl"];
res = Export[FileNameJoin[{generatedWLDir, "Kernel", "AcceptableOperators.wl"}], Column[acceptableOperatorsWL], "String"];

Print[res];

If[FailureQ[res],
  Quit[1]
];

Print["Done Acceptable Operators"]
)

If[!StringQ[script],
  Quit[1]
]
If[AbsoluteFileName[script] === AbsoluteFileName[$InputFileName],
generate[]
]

End[]

EndPackage[]
