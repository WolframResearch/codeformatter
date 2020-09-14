(* Wolfram Language Test file *)

Needs["CodeFormatter`"]





(*
Non-contiguous brackets
*)
Test[
	CodeFormat["f[ [1]]"]
	,
	"f[[1]]"
	,
	TestID -> "CodeFormatter-20191111-O8Y0I1"
]

Test[
	CodeFormat["f[[1] ]"]
	,
	"f[[1]]"
	,
	TestID -> "CodeFormatter-20191111-X2P2T6"
]

Test[
	CodeFormat["f[[1] ]"]
	,
	"f[[1]]"
	,
	TestID -> "CodeFormatter-20191111-C0H0R2"
]




(*
Bad UTF-8 encoding
*)
Test[
	CodeFormat[{206}]
	,
	"\[UnknownGlyph]"
	,
	TestID -> "CodeFormatter-20191111-W3S9H1"
]



(*
Top-level line continuations
*)
Test[
	CodeFormat["{a \\\n+1}"]
	,
	"{a + 1}"
	,
	TestID -> "CodeFormatter-20191111-P7F7L2"
]






Test[
	CodeFormat["1.2`->3"]
	,
	"1.2` -> 3"
	,
	TestID -> "CodeFormatter-20191111-H4M7F5"
]

Test[
	CodeFormat["a-->0"]
	,
	"a-- > 0"
	,
	TestID -> "CodeFormatter-20191111-O9V1N1"
]

Test[
	CodeFormat["a--=0"]
	,
	"a-- = 0"
	,
	TestID -> "CodeFormatter-20191111-H2F5P8"
]

Test[
	CodeFormat["<||>=0"]
	,
	"<||> = 0"
	,
	TestID -> "CodeFormatter-20191111-F4G3N8"
]

Test[
	CodeFormat["t/.3"]
	,
	"t / .3"
	,
	TestID -> "CodeFormatter-20191111-L7X1O9"
]

Test[
	CodeFormat["a++=0"]
	,
	"a++ = 0"
	,
	TestID -> "CodeFormatter-20191111-F4G6G5"
]







Test[
	CodeFormat["a = ."]
	,
	"a = ."
	,
	TestID -> "CodeFormatter-20191111-M7F5A1"
]







Test[
	CodeFormat["a "]
	,
	"a"
	,
	TestID -> "CodeFormatter-20191111-C8L0H3"
]







(*
Comments are not modified
*)
Test[
	CodeFormat["(*a\\\nb*)c"]
	,
	"(*a\\\nb*)c"
	,
	TestID -> "CodeFormatter-20191112-S2I3Y2"
]



Test[
	CodeFormat["0.."]
	,
	"0 .."
	,
	TestID -> "CodeFormatter-20191113-P5R8O4"
]





Test[
	CodeFormat["_..."]
	,
	"_. .."
	,
	TestID -> "CodeFormatter-20191113-T8E6X9"
]






(*
Implicit Times
*)

Test[
	CodeFormat["1.2`a"]
	,
	"1.2` a"
	,
	TestID -> "CodeFormatter-20191111-H8F1J5"
]

Test[
	CodeFormat["1.2` a"]
	,
	"1.2` a"
	,
	TestID -> "CodeFormatter-20191114-A1W5Y9"
]

Test[
	CodeFormat["_.0"]
	,
	"_. 0"
	,
	TestID -> "CodeFormatter-20191113-C0N8Q4"
]

Test[
	CodeFormat["_. 0"]
	,
	"_. 0"
	,
	TestID -> "CodeFormatter-20191114-Y5X5K7"
]

Test[
	CodeFormat["a_.b"]
	,
	"a_. b"
	,
	TestID -> "CodeFormatter-20191113-K9N4C8"
]

Test[
	CodeFormat["a_. b"]
	,
	"a_. b"
	,
	TestID -> "CodeFormatter-20191114-C8O3Z3"
]

Test[
	CodeFormat["____"]
	,
	"___ _"
	,
	TestID -> "CodeFormatter-20191113-X8H4W7"
]

Test[
	CodeFormat["___ _"]
	,
	"___ _"
	,
	TestID -> "CodeFormatter-20191114-A8N3P3"
]


Test[
	CodeFormat["iMelToHz[args_,opts_]"]
	,
	"iMelToHz[args_, opts_]"
	,
	TestID -> "CodeFormatter-20191121-K3W7G3"
]



Test[
	CodeFormat[
"
foo[bar[[1, 2]
]]
"]
 	,
 	"foo[bar[[1, 2]]]"
	,
	TestID->"CodeFormatter-20200406-V9L3L5"
]


Test[
	CodeFormat["{0. ...}"]
	,
	"{0. ...}"
	,
	TestID->"CodeFormatter-20200715-K5E0Q1"
]



Test[
	CodeFormat["a\"\t\""]
	,
	"a \"  \""
	,
	TestID->"CodeFormatter-20200803-Y4E5E2"
]



Test[
	CodeFormat["a(*\t*)&"]
	,
	"a(* *)&"
	,
	TestID->"CodeFormatter-20200803-U6J1P6"
]


Test[
	CodeFormat["If[a, b, c]"]
	,
	"If[a, b, c]"
	,
	TestID->"CodeFormatter-20200805-Q8H6M7"
]


TestMatch[
	CodeFormat["\\((*\\ \\(Test\\ comment\\)\\ *)\\)"]
	,
	_String
	,
	TestID->"CodeFormatter-20200813-B6S4H2"
]







