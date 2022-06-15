(* Wolfram Language Test file *)

Needs["CodeFormatter`"]

Needs["CodeParser`"]



(*
bug 398836
*)
cst = CodeConcreteParseBox[RowBox[{"a", " ", "b"}]]

Test[
	CodeFormatCST[cst]
	,"a b\n"
	,
	TestID->"Bugs-20201016-T9R8A5"
]





Test[
	CodeFormat["1 + 2222222222222222222222222 + 3", "LineWidth" -> 20, "BreakLinesMethod" -> "LineBreakerV1"]
	,
"1 + 2222222222222222222222222 +
     3"
	,
	TestID->"Bugs-20201016-J1C9L9"
]





(*
Bug 404109
*)

input = ToString[Nest[f, x, 30], InputForm, PageWidth -> Infinity]

Test[
	CodeFormat[input, "LineWidth" -> 20, "BreakLinesMethod" -> "LineBreakerV1"]
	,
"f[f[f[f[f[f[
    f[f[f[f[f[f[
    f[f[f[f[f[f[
    f[f[f[f[f[f[
    f[f[f[f[f[f[
    x]]]]]]]]]]
    ]]]]]]]]]]]
    ]]]]]]]]]"
	,
	TestID->"Bugs-20210112-W2I4Y2"
]




input = StringJoin["f[", Table["#1 ", {40}], "]"]

Test[
	CodeFormat[input, "LineWidth" -> 20, "BreakLinesMethod" -> "LineBreakerV1"]
	,
"f[#1 #1 #1 
    #1 #1 #1 #1
     #1 #1 #1 #1
     #1 #1 #1 #1
     #1 #1 #1 #1
     #1 #1 #1 #1
     #1 #1 #1 #1
     #1 #1 #1 #1
     #1 #1 #1 #1
     #1 #1 #1 #1
     #1]"
	,
	TestID->"Bugs-20210112-U7K8N2"
]


(*
is this strange?
*)
Test[
	CodeFormat[input, "LineWidth" -> 20, "BreakLinesMethod" -> "LineBreakerV2"]
	,
"f[
    #1
    
    #1
    
    #1
    
    #1
    
    #1
    
    #1
    
    #1
    
    #1
    
    #1
    
    #1
    
    #1
    
    #1
    
    #1
    
    #1
    
    #1
    
    #1
    
    #1
    
    #1
    
    #1
    
    #1
    
    #1
    
    #1
    
    #1
    
    #1
    
    #1
    
    #1
    
    #1
    
    #1
    
    #1
    
    #1
    
    #1
    
    #1
    
    #1
    
    #1
    
    #1
    
    #1
    
    #1
    
    #1
    
    #1
    
    #1
]"
	,
	TestID->"Bugs-20211007-C8M2O4"
]


input =
"f[
\"1:eJxTTMoPSmVmYGBgAmJeKA0Big4Wru5FP/n1HU4vdN32uVYIzpfQv6vC1vjd
3kzqQLTCQQ04X/r1IzMpoD4Yn6mCW0WjTgiuv+N0vcf+2u/2MPNhfCmQeqA5
ML5sVIr1faA9MP0wPsx8GB9mP0w/zH3o7ofxG4JLVKb/f2DP3jjVuTtHAM4/
AXLf3wf2NuHRG/cDxWF8mH0w/gwtialXTmo6wPTv/NP+5Xa5Adx8GJ/r+uIC
21OacP6nS75JAhUGDjD9MD7MfBgfZj9MP8x96O4HAGYxoDo=\"]"

Test[
	CodeFormat[input, "LineWidth" -> 20, "BreakLinesMethod" -> "LineBreakerV1"]
	,
"f[\"1:eJxTTMoPSmVmYGBgAmJeKA0Big4Wru5FP/n1HU4vdN32uVYIzpfQv6vC1vjd
3kzqQLTCQQ04X/r1IzMpoD4Yn6mCW0WjTgiuv+N0vcf+2u/2MPNhfCmQeqA5
ML5sVIr1faA9MP0wPsx8GB9mP0w/zH3o7ofxG4JLVKb/f2DP3jjVuTtHAM4/
AXLf3wf2NuHRG/cDxWF8mH0w/gwtialXTmo6wPTv/NP+5Xa5Adx8GJ/r+uIC
21OacP6nS75JAhUGDjD9MD7MfBgfZj9MP8x96O4HAGYxoDo=\"
    ]"
	,
	TestID->"Bugs-20210113-K8Y8F3"
]

Test[
	CodeFormat[input, "LineWidth" -> 20, "BreakLinesMethod" -> "LineBreakerV2"]
	,
"f[
\"1:eJxTTMoPSmVmYGBgAmJeKA0Big4Wru5FP/n1HU4vdN32uVYIzpfQv6vC1vjd
3kzqQLTCQQ04X/r1IzMpoD4Yn6mCW0WjTgiuv+N0vcf+2u/2MPNhfCmQeqA5
ML5sVIr1faA9MP0wPsx8GB9mP0w/zH3o7ofxG4JLVKb/f2DP3jjVuTtHAM4/
AXLf3wf2NuHRG/cDxWF8mH0w/gwtialXTmo6wPTv/NP+5Xa5Adx8GJ/r+uIC
21OacP6nS75JAhUGDjD9MD7MfBgfZj9MP8x96O4HAGYxoDo=\"
]"
	,
	TestID->"Bugs-20211007-F2G5T9"
]







(*
bug 407712
*)
Test[
	CodeFormat["unSetMyGlobal:=(MyGlobal=.)"]
	,
"unSetMyGlobal :=
    (MyGlobal =.)"
	,
	TestID->"Bugs-20210406-H6V8N9"
]






(*
bug 408992
*)
Test[
	CodeFormat["a:b"]
	,
	"a:b"
	,
	TestID->"Bugs-20210426-L8Q5O5"
]

Test[
	CodeFormat["a:b|c"]
	,
	"a : b | c"
	,
	TestID->"Bugs-20210426-P4B2D7"
]





(*
bug 409143
*)
Test[
	CodeFormat["foo::bars"]
	,
	"foo::bars"
	,
	TestID->"Bugs-20210428-I2Z4W1"
]



(*
bug 414042

it is ok if this changes format

just need to make sure that & is not on a new line
*)
Test[
	CodeFormat[
 "f[] := Block[{}, Block[{},
	If[validationQ,
		datasetValidation[[\"BoundingBox\"]] = MapThread[PadCorner[#] /@ #2&,
		{datasetValidation[[\"Size\"]], datasetValidation[[\"BoundingBox\"]]}];
	];
]]", "IndentationString" -> "\t", "BreakLinesMethod" -> "LineBreakerV1"]
	,
"f[] :=
	Block[{},
		Block[{},
			If[validationQ,
				datasetValidation[[\"BoundingBox\"]] = MapThread[PadCorner[#] /@ #2&,
					 {datasetValidation[[\"Size\"]], datasetValidation[[\"BoundingBox\"]]}];
			];
		]
	]"
	,
	TestID->"Bugs-20210903-D6C3Y9"
]

Test[
	CodeFormat[
 "f[] := Block[{}, Block[{},
	If[validationQ,
		datasetValidation[[\"BoundingBox\"]] = MapThread[PadCorner[#] /@ #2&,
		{datasetValidation[[\"Size\"]], datasetValidation[[\"BoundingBox\"]]}];
	];
]]", "IndentationString" -> "\t", "BreakLinesMethod" -> "LineBreakerV2"]
	,
"f[] :=
	Block[{},
		Block[{},
			If[validationQ,
				datasetValidation[[\"BoundingBox\"]] =
					MapThread[
						PadCorner[#] /@ #2&
						,
						{datasetValidation[[\"Size\"]], datasetValidation[[\"BoundingBox\"]]}
					];
			];
		]
	]"
	,
	TestID->"Bugs-20211007-A3T2R2"
]







(*
was giving StringLength::string messages
*)
TestMatch[
	CodeFormat["(Sum[a b c, {i, 1, 9}](*+
xxx*))
"]
	,
"(
    Sum[a b c, {i, 1, 9}](*+
xxx*) )"
	,
	TestID->"Bugs-20210929-O7G2W2"
]







(*
bug 417300
*)

cst = CodeConcreteParse["add[a_, b_]"]

first = cst[[2, 1]]

Test[
	CodeFormatCST[first]
	,
"add[a_, b_]
"
	,
	TestID->"Bugs-20211117-D2N8Q0"
]

cst = CodeConcreteParse["(a+b)"]

first = cst[[2, 1]]

Test[
	CodeFormatCST[first]
	,
"(a + b)
"
	,
	TestID->"Bugs-20211117-K3G2M8"
]






(*
bug 417935
*)
Test[
	CodeFormat["Module[{x = 10}, x + 1]", "NewlinesInScoping" -> Insert]
	,
	"\
Module[
    {x = 10}
    ,
    x + 1
]"
	,
	TestID->"Bugs-20211202-Z3N3I9"
]

Test[
	CodeFormat["With[{a=1},{b=2},{c=3},a]", "NewlinesInScoping" -> Insert]
	,
	"\
With[
    {a = 1}
    ,
    {b = 2}
    ,
    {c = 3}
    ,
    a
]"
	,
	TestID->"Bugs-20211202-X4S6G6"
]

Test[
	CodeFormat["If[a,b,c]", "NewlinesInControl" -> Insert]
	,
	"\
If[
    a
    ,
    b
    ,
    c
]"
	,
	TestID->"Bugs-20211202-V3O6H9"
]

Test[
	CodeFormat["Switch[a,b,c,d,e]", "NewlinesInControl" -> Insert]
	,
	"\
Switch[
    a
    ,
    b
    ,
    c
    ,
    d
    ,
    e
]"
	,
	TestID->"Bugs-20211202-C8Q5W0"
]

Test[
	CodeFormat["Which[a,b,c,d]", "NewlinesInControl" -> Insert]
	,
	"\
Which[
    a
    ,
    b
    ,
    c
    ,
    d
]"
	,
	TestID->"Bugs-20211202-N0K6F5"
]

Test[
	CodeFormat["For[start,test,inc,body]", "NewlinesInControl" -> Insert]
	,
	"\
For[
    start
    ,
    test
    ,
    inc
    ,
    body
]"
	,
	TestID->"Bugs-20211202-B7O2N0"
]


cst =
  ContainerNode[
   String, {CallNode[{LeafNode[Symbol, "Module", <||>]}, GroupNode[
       GroupSquare, {LeafNode[Token`OpenSquare, "[", <||>],
        InfixNode[
         Comma, {GroupNode[
           List, {LeafNode[Token`OpenCurly, "{", <||>],
            InfixNode[
             Comma, {LeafNode[Symbol, "a", <||>],
              LeafNode[Token`Comma, ",", <||>],
              LeafNode[Symbol, "b", <||>]}, <||>],
            LeafNode[Token`CloseCurly, "}", <||>]}, <||>],
          LeafNode[Token`Comma, ",", <||>],
          LeafNode[Symbol, "foo", <||>]}, <||>],
        LeafNode[Token`CloseSquare, "]", <||>]}, <||>], <||>]}, <||>]

Test[
	CodeFormatCST[cst]
	,
	"\
Module[{a, b},
    foo
]
"
	,
	TestID->"Bugs-20220429-S6P7I8"
]


