
Needs["CodeFormatter`"]

str = "
$notebookStatusToWeight = {
	\"ObsoleteFlag\" -> 0.000001, (* don't use 10^-6 or Rational[...] will cause issues in the web deployed search index *)
	\"AwaitingFutureDesignReviewFlag\" -> .25,
	\"NewInOldVersion\" -> 0.00001,
	\"None\" -> 1.
};
"

(*
Just verify that sanity check passed
*)
TestMatch[
	CodeFormat[str, "LineWidth" -> 120]
	,
	_String
	,
	TestID->"LineBreaking-20200804-Z9K7C7"
]




(*
Handle complex continuations
*)

str = "
LaunchKernels::unicore = \"The default parallel kernel configuration does not launch any kernels on a single-core machine.\\
\tUse LaunchKernels[n] to launch n kernels anyway.\"
";
  
(*
Just verify that sanity check passed
*)
TestMatch[
	CodeFormat[str, "LineWidth" -> 120]
	,
	_String
	,
	TestID->"LineBreaking-20200804-J5A7C1"
]


str = "
{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, \\[Infinity]}
";

(*
Just verify that sanity check passed
*)
TestMatch[
	CodeFormat[str, "LineWidth" -> 120]
	,
	_String
	,
	TestID->"LineBreaking-20200804-Z8Q2C4"
]


str = "\"0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 \\\"\""

(*
Just verify that sanity check passed
*)
TestMatch[
	CodeFormat[str, "LineWidth" -> 120]
	,
	_String
	,
	TestID->"LineBreaking-20200804-U4J1K2"
]




(*
Harder to test splitting up long names

Splitting up a long name does not result in bad syntax and sanity check failure

So do a direct test for the continuation being placed before the long name
*)
str = "\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\\[DiscretionaryParagraphSeparator]\"";

Test[
	Block[{CodeFormatter`BreakLines`Private`$AllowSplittingTokens = True},
		CodeFormat[str, "LineWidth" -> 120]
	]
	,
	"\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\\\n\\[DiscretionaryParagraphSeparator]\""
	,
	TestID->"LineBreaking-20200805-A0T7P8"
]




(*
Make sure to handle multiple escapes
*)
str = "\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\\\"\\\"\""

TestMatch[
	CodeFormat[str, "LineWidth" -> 120]
	,
	_String
	,
	TestID->"LineBreaking-20200805-S2Y1J5"
]



(*
Test breaking on \[
*)
str = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa + \\[FormalX]"

TestMatch[
	CodeFormat[str, "LineWidth" -> 120]
	,
	_String
	,
	TestID->"LineBreaking-20200805-W8K3O3"
]

(*
Test breaking on \:
*)
str = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa + \\:03b1"

TestMatch[
	CodeFormat[str, "LineWidth" -> 120]
	,
	_String
	,
	TestID->"LineBreaking-20200810-T5K2E1"
]







(*
Test breaking on implicit Times
*)

str = "(aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa b)"

TestMatch[
	Block[{CodeFormatter`Private`$DisableSanityChecking = True,
		CodeFormatter`BreakLines`Private`$AllowSplittingTokens = True},
		CodeFormat[str, "LineWidth" -> 120]
	]
	,
"(aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
     b)"
	,
	{}
	,
	TestID->"LineBreaking-20200805-S9F1H1"
]




(*
Test nested comments
*)
TestMatch[
	CodeFormat["(*(**)*)", "LineWidth" -> 5]
	,
	_String
	,
	TestID->"LineBreaking-20200808-F9R3L9"
]



(*
Test sequences of longnames
*)

str =
"
lowcaseQ :=
    StringMatchQ[s, RegularExpression[\"[\\[Alpha]\\[Beta]\\[Gamma]\\[Delta]\\[Epsilon]\\[CurlyKappa]\\[Lambda]\\[Mu]\\[Nu]\\[Xi]\\[Omicron]\\[Pi]\\[CurlyPi]\\[Rho]\\[Chi]\\[Psi]]\"]]
"

TestMatch[
	CodeFormat[str, "LineWidth" -> 40, "SafetyMargin" -> 10, Airiness -> 0]
	,
	_String
	,
	TestID->"LineBreaking-20200811-V0V3B3"
]



