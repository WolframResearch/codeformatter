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
	CodeFormat["1 + 2222222222222222222222222 + 3", "LineWidth" -> 20]
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

formatted = CodeFormat[input, "LineWidth" -> 20]

Test[
	!FailureQ[formatted]
	,
	True
	,
	TestID->"Bugs-20210112-W2I4Y2"
]

lines = StringSplit[formatted, "\n"]

Test[
	AllTrue[lines, (StringLength[#] <= 20)&]
	,
	True
	,
	TestID->"Bugs-20210112-M6K5E2"
]


input = StringJoin["f[", Table["#1 ", {40}], "]"]

formatted = CodeFormat[input, "LineWidth" -> 20]

Test[
	!FailureQ[formatted]
	,
	True
	,
	TestID->"Bugs-20210112-U7K8N2"
]

lines = StringSplit[formatted, "\n"]

Test[
	AllTrue[lines, (StringLength[#] <= 20)&]
	,
	True
	,
	TestID->"Bugs-20210112-S5Z1M8"
]


input =
"f[
\"1:eJxTTMoPSmVmYGBgAmJeKA0Big4Wru5FP/n1HU4vdN32uVYIzpfQv6vC1vjd
3kzqQLTCQQ04X/r1IzMpoD4Yn6mCW0WjTgiuv+N0vcf+2u/2MPNhfCmQeqA5
ML5sVIr1faA9MP0wPsx8GB9mP0w/zH3o7ofxG4JLVKb/f2DP3jjVuTtHAM4/
AXLf3wf2NuHRG/cDxWF8mH0w/gwtialXTmo6wPTv/NP+5Xa5Adx8GJ/r+uIC
21OacP6nS75JAhUGDjD9MD7MfBgfZj9MP8x96O4HAGYxoDo=\"]"

formatted = CodeFormat[input, "LineWidth" -> 20]

Test[
	!FailureQ[formatted]
	,
	True
	,
	TestID->"Bugs-20210113-K8Y8F3"
]

(*lines = StringSplit[formatted, "\n"]

Test[
	AllTrue[lines, (StringLength[#] <= 20)&]
	,
	True
	,
	TestID->"Bugs-20210113-Y6R5G3"
]
*)








(*
bug 407712
*)
Test[
	CodeFormat["unSetMyGlobal:=(MyGlobal=.)"]
	,
	"\
unSetMyGlobal :=
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








