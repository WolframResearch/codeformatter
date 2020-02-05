(* Wolfram Language Test file *)

Needs["CodeFormatter`"]





(*
Non-contiguous brackets
*)
Test[
	FormatString["f[ [1]]"]
	,
	"f[[1]]"
	,
	TestID -> "Format-20191111-O8Y0I1"
]

Test[
	FormatString["f[[1] ]"]
	,
	"f[[1]]"
	,
	TestID -> "Format-20191111-X2P2T6"
]

Test[
	FormatString["f[[1] ]"]
	,
	"f[[1]]"
	,
	TestID -> "Format-20191111-C0H0R2"
]




(*
Bad UTF-8 encoding
*)
Test[
	FormatBytes[{206}]
	,
	ToCharacterCode["\[UnknownGlyph]", "UTF8"]
	,
	{FormatBytes::noaction}
	,
	TestID -> "Format-20191111-W3S9H1"
]






Test[
	FormatString["{a\rb}"]
	,
	"{a\rb}"
	,
	{FormatString::noaction}
	,
	TestID -> "Format-20191111-N8K0J2"
]





(*
Top-level line continuations
*)
Test[
	FormatString["{a \\\n+1}"]
	,
	"{a\n+1}"
	,
	TestID -> "Format-20191111-P7F7L2"
]






Test[
	FormatString["1.2`->3"]
	,
	"1.2` ->3"
	,
	TestID -> "Format-20191111-H4M7F5"
]

Test[
	FormatString["a-->0"]
	,
	"a-- >0"
	,
	TestID -> "Format-20191111-O9V1N1"
]

Test[
	FormatString["a--=0"]
	,
	"a-- =0"
	,
	TestID -> "Format-20191111-H2F5P8"
]

Test[
	FormatString["<||>=0"]
	,
	"<||> =0"
	,
	TestID -> "Format-20191111-F4G3N8"
]

Test[
	FormatString["t/.3"]
	,
	"t/ .3"
	,
	TestID -> "Format-20191111-L7X1O9"
]

Test[
	FormatString["a++=0"]
	,
	"a++ =0"
	,
	TestID -> "Format-20191111-F4G6G5"
]







Test[
	FormatString["a = ."]
	,
	"a =."
	,
	TestID -> "Format-20191111-M7F5A1"
]







Test[
	FormatString["a "]
	,
	"a"
	,
	TestID -> "Format-20191111-C8L0H3"
]







(*
Comments are not modified
*)
Test[
	FormatString["(*a\\\nb*)c"]
	,
	"(*a\\\nb*)c"
	,
	TestID -> "Format-20191112-S2I3Y2"
]






Test[
	FormatString["{ f\n& }"]
	,
	"{ f& }"
	,
	TestID -> "Format-20191113-I8N9T2"
]





Test[
	FormatString["0.."]
	,
	"0 .."
	,
	TestID -> "Format-20191113-P5R8O4"
]





Test[
	FormatString["_..."]
	,
	"_ ..."
	,
	TestID -> "Format-20191113-T8E6X9"
]






(*
Implicit Times
*)

Test[
	FormatString["1.2`a"]
	,
	"1.2` a"
	,
	TestID -> "Format-20191111-H8F1J5"
]

Test[
	FormatString["1.2` a"]
	,
	"1.2` a"
	,
	TestID -> "Format-20191114-A1W5Y9"
]

Test[
	FormatString["_.0"]
	,
	"_. 0"
	,
	TestID -> "Format-20191113-C0N8Q4"
]

Test[
	FormatString["_. 0"]
	,
	"_. 0"
	,
	TestID -> "Format-20191114-Y5X5K7"
]

Test[
	FormatString["a_.b"]
	,
	"a_. b"
	,
	TestID -> "Format-20191113-K9N4C8"
]

Test[
	FormatString["a_. b"]
	,
	"a_. b"
	,
	TestID -> "Format-20191114-C8O3Z3"
]

Test[
	FormatString["____"]
	,
	"___ _"
	,
	TestID -> "Format-20191113-X8H4W7"
]

Test[
	FormatString["___ _"]
	,
	"___ _"
	,
	TestID -> "Format-20191114-A8N3P3"
]


Test[
	FormatString["iMelToHz[args_,opts_]"]
	,
	"iMelToHz[args_, opts_]"
	,
	TestID -> "Format-20191121-K3W7G3"
]


Test[
	FormatString["{a \n&}"]
	,
	"{a&}"
	,
	TestID -> "Format-20191121-K9O8I8"
]











