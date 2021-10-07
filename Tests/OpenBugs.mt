
Needs["CodeFormatter`"]

Needs["CodeParser`"]


(*
OPEN BUG: Airiness -> 1 and anchored comments that absorb newlines do not play well together
*)
Test[
	CodeFormat["If(*1*)[(*2*)a(*3*),(*4*)b(*5*)]", Airiness -> 1]
	,
"If(*1*)[
    (*2*)
    a
    (*3*)
    ,
    (*4*)
    b
    (*5*)
]"
	,
	TestID->"OpenBugs-20210113-C5B5D4"
]









(*

OPEN BUG

current limitations of LineBreakerV2:

line ends up being 91 characters long because:

"DeleteMissing[publisherResourceNameSpace /@ allPublisherInfo[][\"Publishers\"]]"" = 77 characters, so 1 line

"ns = DeleteMissing[publisherResourceNameSpace /@ allPublisherInfo[][\"Publishers\"]]"" = 82 characters, so multiline

IndentationNode[Block, {the above code}] = 81 characters

nothing ever forces first line to actually re-indent


It doesn't really matter what the actual result is
The test is for <80 character lines
*)

Test[
	CodeFormat[	
"resourcePublisherNameSpaceFreeQ[name_String] :=
    With[
        {
            ns
            =
            DeleteMissing[publisherResourceNameSpace /@ allPublisherInfo[][\"Publishers\"]]
        }
        ,
        !MatchQ[name, Alternatives @@ ns]
    ]"
    ,
    "BreakLinesMethod" -> "LineBreakerV2"
	]
	,
"resourcePublisherNameSpaceFreeQ[name_String] :=
    With[
        {
            ns
            =
            DeleteMissing[
			    publisherResourceNameSpace
			    /@
			    allPublisherInfo[][
			        \"Publishers\"
			    ]
			]
        }
        ,
        !MatchQ[name, Alternatives @@ ns]
    ]"
	,
	TestID->"OpenBugs-20211007-J5F4K7"
]



Test[
	CodeFormat["1 + 2222222222222222222222222 + 3", "LineWidth" -> 20, "BreakLinesMethod" -> "LineBreakerV2"]
	,
"1 + 2222222222222222222222222 +
    3"
	,
	TestID->"OpenBugs-20211007-B3W2W2"
]







(*
OPEN BUG

LineBreakerV2

if indenting would go over line width, then there should be a way to not indent

*)


input = ToString[Nest[f, x, 30], InputForm, PageWidth -> Infinity]

Test[
	CodeFormat[input, "LineWidth" -> 20, "BreakLinesMethod" -> "LineBreakerV2"]
	,
"f[
f[
f[
f[
f[
f[
f[
f[
f[
f[
f[
f[
f[
f[
f[
f[
f[
f[
f[
f[
f[
f[
f[
f[
f[f[f[f[f[f[x]]]]]]
]
]
]
]
]
]
]
]
]
]
]
]
]
]
]
]
]
]
]
]
]
]
]
]"
	,
	TestID->"OpenBugs-20211007-H9I5O1"
]











(*
OPEN BUG

the Token`Fake`ImplicitTimes is on its own line, but it is invisible

so there should only be 1 newline
*)

TestMatch[
	CodeFormat[str, "LineWidth" -> 120, "BreakLinesMethod" -> "LineBreakerV2"]
	,
"(
    aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
    b
)"
	,
	{}
	,
	TestID->"OpenBugs-20211007-J3W2U8"
]





(*
OPEN BUG

non-anchored comments should insert spaces where appropriate
*)

Test[
	CodeFormat["{edgeforms (*color*)}"]
	,
	"{edgeforms (*color*)}"
	,
	TestID->"OpenBugs-20211007-V0I8A3"
]











(*
OPEN BUG

comments should be indented properly
*)

Test[
	CodeFormat[
"Module[
    {x}
    ,
(*
a comment
*)
    f[x]
]"]
	,
"Module[
    {x}
    ,
    (*
    a comment
    *)
    f[x]
]"
	,
	TestID->"OpenBugs-20211007-K9U3B8"
]







