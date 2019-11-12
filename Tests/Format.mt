(* Wolfram Language Test file *)

Needs["Format`"]

Test[
	FormatString["f[ [1]]"]
	,
	"f[[1]]"
	,
	TestID->"Format-20191111-O8Y0I1"
]

Test[
	FormatString["f[[1] ]"]
	,
	"f[[1]]"
	,
	TestID->"Format-20191111-X2P2T6"
]

Test[
	FormatString["f[[1] ]"]
	,
	"f[[1]]"
	,
	TestID->"Format-20191111-C0H0R2"
]

Test[
	FormatBytes[{206}]
	,
	ToCharacterCode["\[UnknownGlyph]\n", "UTF8"]
	,
	{FormatBytes::noaction}
	,
	TestID->"Format-20191111-W3S9H1"
]



Test[
	FormatString["{a\rb}"]
	,
	"{a\rb}"
	,
	{FormatString::noaction}
	,
	TestID->"Format-20191111-N8K0J2"
]



Test[
	FormatString["{a \\\n+1}"]
	,
	"{a\n+1}"
	,
	TestID->"Format-20191111-P7F7L2"
]


Test[
	FormatString["1.2`a"]
	,
	"1.2` a"
	,
	TestID->"Format-20191111-H8F1J5"
]

Test[
	FormatString["1.2`->3"]
	,
	"1.2` ->3"
	,
	TestID->"Format-20191111-H4M7F5"
]

Test[
	FormatString["a-->0"]
	,
	"a-- >0"
	,
	TestID->"Format-20191111-O9V1N1"
]

Test[
	FormatString["a--=0"]
	,
	"a-- =0"
	,
	TestID->"Format-20191111-H2F5P8"
]

Test[
	FormatString["<||>=0"]
	,
	"<||> =0"
	,
	TestID->"Format-20191111-F4G3N8"
]

Test[
	FormatString["t/.3"]
	,
	"t/ .3"
	,
	TestID->"Format-20191111-L7X1O9"
]

Test[
	FormatString["a++=0"]
	,
	"a++ =0"
	,
	TestID->"Format-20191111-F4G6G5"
]

Test[
	FormatString["a = ."]
	,
	"a =."
	,
	TestID->"Format-20191111-M7F5A1"
]







Test[
	FormatString["a "]
	,
	"a"
	,
	TestID->"Format-20191111-C8L0H3"
]








Test[
	FormatString["(*a\\\nb*)c"]
	,
	"(*a\nb*)c"
	,
	TestID->"Format-20191112-S2I3Y2"
]




