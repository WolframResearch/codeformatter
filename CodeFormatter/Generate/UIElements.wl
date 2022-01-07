(* ::Package::"Tags"-><|"NoVariables" -> <|"DynamicModule" -> <|Enabled -> False|>|>|>:: *)

(* ::Package:: *)

(* ::Section::Closed:: *)
(*Package Header*)


Unprotect["CodeFormatter`Generate`UIElements`*"];


ClearAll["CodeFormatter`Generate`UIElements`*"];


BeginPackage["CodeFormatter`Generate`UIElements`"];


(* ::Section::Closed:: *)
(*Appearance*)


tr[name_String] := Dynamic[FEPrivate`FrontEndResource["CodeFormatterStrings", name]]
tre[name_String] := Dynamic[RawBoxes[FEPrivate`FrontEndResource["CodeFormatterExpressions", name]]]


CodeFormatter`$optsPath = Splice[{CodeAssistOptions, "CodeToolsOptions", "CodeFormat"}];


BackgroundCol = GrayLevel[.95];


Chevron[Dynamic[direction_]] :=
Graphics[
	{
		AbsoluteThickness[2], JoinForm["Round"], CapForm["Round"],
		Style[
			Dynamic[
				Line @ Switch[direction,
					Left,  {{.5, -1}, {-.5, 0}, {.5, 1}},
					Right, {{-.5, -1}, {.5, 0}, {-.5, 1}},
					Up,    {{-1, -.5}, {0, .5}, {1, -.5}},
					_,     {{-1, .5}, {0, -.5}, {1, .5}}]],
			"CodeFormatterHighlightColor"]},
	ImageSize -> {10, 10}, ImageMargins -> {{0, 1}, {1, 0}}, BaselinePosition -> Bottom
]


dialogSuggestedWidth = 240;


delimiter = Graphics[
	{
		AbsoluteThickness[1], GrayLevel[.6], CapForm["Round"],
		Line[{{0, 0}, {0, 1}}]
	},
	ImageSize -> {32, 18}, AspectRatio -> Full
];


delimiter2 =
Graphics[
	{GrayLevel[.8], AbsoluteThickness[1], CapForm["Round"], Line[{{0, 0}, {1, 0}}]},
	AspectRatio -> Full, ImageSize -> {213, 12}]


opener[label_, content_] := 
With[{grid = Grid[{{##}}, ItemSize -> {0, 0}, Spacings -> 0]&},
	DynamicModule[{openQ = False},
		grid[
			Button[
				grid[label, Spacer[3], Chevron[Dynamic[Switch[openQ, True, Left, False, Right]]]],
				openQ = !openQ, Appearance -> None
			],
			PaneSelector[{True -> grid[Spacer[8], content], False -> Spacer[{0, 0}]}, Dynamic[openQ], ImageSize -> Automatic]
		],
		BaseStyle -> "CodeFormatterText"
	]
]


(*
	If the paclet targetd 13.0+ then we could use the new Dialog.nb styles and "RoundedRectangleEnabledButtonAppearance" in MiscExpressions.tr.
	Moreover, "SuppressMouseDownNinePatchAppearance" did not exist until 12.2+.
	Instead we copy the 13.0 Dialog.nb and the text resource from 12.2+ into this paclet to continue targeting 12.1+. *)
roundedRectButtonAppearance[content_, type_, Dynamic[enabledCondition_], frameBoxOptions___] :=
DynamicModule[{mouseUp = True},
	EventHandler[
		PaneSelector[
			{
				"Disabled" ->
					Framed[
						Style[content, "CodeFormatterTextBase"],
						BaseStyle -> FEPrivate`StringJoin["Button", type, "Disabled"],
						frameBoxOptions],
				"Default" ->
					Framed[
						Style[content, "CodeFormatterTextBase"],
						BaseStyle -> FEPrivate`StringJoin["Button", type, "Normal"],
						frameBoxOptions],
				"Hover" ->
					Framed[
						Style[content, "CodeFormatterTextBase"],
						BaseStyle -> FEPrivate`StringJoin["Button", type, "Hover"],
						frameBoxOptions],
				"Pressed" ->
					Framed[
						Style[content, "CodeFormatterTextBase"],
						BaseStyle -> FEPrivate`StringJoin["Button", type, "Pressed"],
						frameBoxOptions]},
			Dynamic[
				Which[
					Not[enabledCondition],                "Disabled",
					CurrentValue["MouseOver"] && mouseUp, "Hover",
					CurrentValue["MouseOver"],            "Pressed",
					True,                                 "Default"
				]
			],
			ImageSize -> Automatic],
		{
			"MouseDown" :> FEPrivate`Set[mouseUp, False],
			"MouseUp" :> FEPrivate`Set[mouseUp, True]},
		PassEventsDown -> True]]


extractEnabledCondition[opts_List] :=
FirstCase[
	opts,
	HoldPattern[(Rule|RuleDelayed)[Enabled, d_]] :> If[MatchQ[d, _Dynamic], d, Dynamic[d]],
	Dynamic[True]]


Attributes[roundedRectButton] = {HoldFirst};

roundedRectButton[action_, content_, colorType_String, {buttonOpts___}, {frameOpts___}] :=
Button[
	roundedRectButtonAppearance[content, colorType, extractEnabledCondition[{buttonOpts}], frameOpts],
	action,
	buttonOpts]


buttonAppearance[label_, size_:{Full, 25}] :=
Framed[
	label,
	RoundingRadius -> 3, ImageSize -> size, Alignment -> Center, Background -> White,
	FrameStyle -> Dynamic @ If[CurrentValue["MouseOver"],
		Directive["CodeFormatterHighlightColor", AbsoluteThickness[1]],
		Directive[GrayLevel[.7], AbsoluteThickness[1]]],
	BaseStyle -> "CodeFormatterText"
]


(* ::Section::Closed:: *)
(*Format Button*)


FormatButton[size_:{Full, 25}] :=
roundedRectButton[
	If[$VersionNumber >= 12.2, CodeFormatter`Notebooks`formatSelectedCell[]],
	tr["FormatButton"],
	"Gray2",
	{
		Method -> "Queued",
		Enabled -> True,
		Appearance -> FEPrivate`FrontEndResource["CodeFormatterExpressions", "SuppressMouseDownNinePatchAppearance"]},
	{ImageSize -> size}]


(* ::Section::Closed:: *)
(*Airiness Slider*)


DenseIcon[col_] := With[{o = {0, 0}, dy = 3.3, dx = 4.3, s = 2},

Graphics[{AbsoluteThickness[1.5], col, CapForm["Butt"],
	Line[{
		{Offset[{0, 2dy - dy/2}, o], Offset[{2dx, 2dy - dy/2}, o]},
		{Offset[{2dx+s, 2dy - dy/2}, o], Offset[{4dx, 2dy - dy/2}, o]},
		
		{Offset[{0, dy - dy/2}, o], Offset[{1dx, dy - dy/2}, o]},
		{Offset[{1dx+s, dy - dy/2}, o], Offset[{3dx, dy - dy/2}, o]},
		{Offset[{3dx+s, dy - dy/2}, o], Offset[{4.5dx, dy - dy/2}, o]},
		
		{Offset[{0, -dy + dy/2}, o], Offset[{1.5dx, -dy + dy/2}, o]},
		{Offset[{1.5dx+s, -dy + dy/2}, o], Offset[{3.8dx, -dy + dy/2}, o]},
		
		{Offset[{0, -2dy + dy/2}, o], Offset[{2.5dx, -2dy + dy/2}, o]},
		{Offset[{2.5dx+s, -2dy + dy/2}, o], Offset[{4dx, -2dy + dy/2}, o]}
	}]
}, ImageSize -> {22, 15}, ImagePadding -> All]

];


AiryIcon[col_] := With[{o = {0, 0}, dy = 3.3, dx = 4.3, s = 2},

Graphics[{AbsoluteThickness[1.5], col, CapForm["Butt"],
	Line[{
		{Offset[{0, 2dy - dy/2}, o], Offset[{1.8dx, 2dy - dy/2}, o]},
		
		{Offset[{1dx, dy - dy/2}, o], Offset[{2.5dx, dy - dy/2}, o]},
		
		{Offset[{2dx, -dy + dy/2}, o], Offset[{3.5dx, -dy + dy/2}, o]},
		
		{Offset[{1dx, -2dy + dy/2}, o], Offset[{2dx, -2dy + dy/2}, o]}
	}]
}, ImageSize -> {17, 15}, ImagePadding -> All]

];

airinessSliderAppearance[Dynamic[airinessValue_], Dynamic[parameters_], Dynamic[highlightQ_]] :=
With[{optVals = CodeFormatter`optionValues},
Graphics[
	{
		(* Draw the slider bar. *)
		AbsoluteThickness[2.5], GrayLevel[.75], CapForm["Round"],
		Line[{Offset[{-8, 0}, {-1, 0}], Offset[{8, 0}, {1, 0}]}],
		(* Ticks *)
		Disk[{#, 0}, Offset[2.5]]& /@ Range[-1, 1, .2],
		(* Handle *)
		FaceForm[White],
		Dynamic[
			EdgeForm[Directive[
				AbsoluteThickness[1],
				If[TrueQ[Replace[FirstPosition[optVals, parameters, {Key[None]}], {Key[n_]} :> n] == airinessValue],
					Dashing[{}]
					,
					AbsoluteDashing[{3, 3}]]]]],
		Dynamic[
			Style[
				Disk[Dynamic[{Replace[airinessValue, Except[_?NumericQ] -> 0.], 0}], Offset[7]],
				If[highlightQ, "CodeFormatterHighlightColor", System`EdgeColor -> GrayLevel[.6]]],
			TrackedSymbols :> {highlightQ}]},
	PlotRange -> {{-1, 1}, {-1, 1}},
	ImagePadding -> {{10, 10}, {0, 0}},
	AspectRatio -> Full,
	ImageSize -> {150, 20}]
]


(*
	A comment on how Airiness and line breaking options work:
	
	CodeFormat takes (among others) a set of options named "NewlinesXXXX", which can be set to False, Automatic, or True. These determine where newlines are inserted.
	CodeFormat also takes an Airiness option, which accepts values between -1 and 1, or Automatic (=0).
	If all "NewlinesXXXX" opts are set to Automatic, then Airiness bank-switches their effective values
	(for example, Airiness -> -1 is equivalent to setting all "NewlinesXXXX" to False,
	Airiness -> 0 is equivalent to setting all "NewlinesXXXX" to Automatic, and
	Airiness -> 1 is equivalent to setting all "NewlinesXXXX" to True).
	However, if a "NewlinesXXXX" opt has been set to True/False, then that value is *not* affected by Airiness.
	So for example, if "NewlinesBetweenCommas" has been set to True, then newlines are *always* inserted between commas *regardless* of the Airiness value.
*)


(* optionValues[airiness] returns the list of values for each newline option for a given airiness. *)
With[{f = False, a = Automatic, t = True},
	CodeFormatter`optionValues = <|
		(* airiness \[Rule] {semicolons, operators, groups, commas, ctrlStruct, scopingStruct, comments} *)
		-1. -> {f, f, f, f, f, f, f},
		-.8 -> {f, f, f, f, f, f, a},
		-.6 -> {f, f, f, a, a, a, a},
		-.4 -> {f, f, a, a, a, a, a},
		-.2 -> {f, a, a, a, a, a, a},
		 0. -> {a, a, a, a, a, a, a},
		 .2 -> {t, a, a, a, a, a, a},
		 .4 -> {t, t, a, a, a, a, a},
		 .6 -> {t, t, t, a, a, a, a},
		 .8 -> {t, t, t, t, t, t, a},
		 1. -> {t, t, t, t, t, t, t}
	|>
];


(* The highlight color is controlled via the style "CodeFormatterHighlightColor" using EdgeColor and FrontFaceColor *)
AirinessSlider[
	Dynamic[airinessValue_, rest___],
	Dynamic[parameters:{semicolons_, operators_, groups_, commas_, ctrlStruct_, scopingStruct_, comments_}]
] :=
With[
	{
		$optsPath = CodeFormatter`$optsPath,
		optionValues = CodeFormatter`optionValues,
		airinessKeys = airinessKeys,
		startFunction = extractFunction["Start", rest],
		dragFunction = extractFunction["Drag", rest],
		mouseUpFunction = extractFunction["End", rest]
	},
	Row[{
		DenseIcon[GrayLevel[.4]],
		DynamicModule[
			{highlightQ = False, pressedQ = False},
			DynamicWrapper[
				EventHandler[
					LocatorPane[
						Dynamic[
							{Replace[airinessValue, Except[_?NumericQ] -> 0.], 0},
							{
								(* mousedown *)
								startFunction[First[#], airinessValue]&,
								(* drag *)
								Composition[
									dragFunction[#, airinessValue]& (* this fires 2nd, assumed to take a single numeric value *)
									,
									Function[(* this fires 1st and includes side effects; it is assumed to return a single numeric value *)
										(* These variables are set while the slider is being dragged, and the CurrentValue assignments
											are left to the end of interaction. This is to ensure the controls are responsive. *)
										parameters = optionValues[Round[Replace[First[#], Except[_?NumericQ] -> 0.], .2]];
										First[#]]
								],
								(* mouse  up*)
								Composition[
									mouseUpFunction[#, airinessValue]& (* this fires 2nd using the return value of the following Function *)
									,
									Function[(* this fires 1st and includes side effects; it is assumed to return a single numeric value *)
										CurrentValue[$FrontEnd, {$optsPath}] = <|
											CurrentValue[$FrontEnd, {$optsPath}, <||>], (* don't blow away non-airiness values *)
											"Airiness" -> First[#],
											"FormatMethod" -> "AirinessSlider",
											AssociationThread[airinessKeys -> parameters]|>;
										First[#]]
								]}],
						airinessSliderAppearance[Dynamic[airinessValue], Dynamic[parameters], Dynamic[highlightQ]],
						(* Allowed locator region. *)
						{{-1, 0}, {1, 0}, {.2, 1}},
						Appearance -> None],
					{
						"MouseDown" :> FEPrivate`Set[pressedQ, True],
						"MouseUp" :> FEPrivate`Set[pressedQ, False]},
					PassEventsDown -> True],
				
				(* Highlight the handle when the slider is hovered over or clicked/dragged. *)
			highlightQ = CurrentValue["MouseOver"] || pressedQ]],
		AiryIcon[GrayLevel[.4]]
		}]
]


(* ::Section::Closed:: *)
(*RoundedSetterBar*)


(*
	There are effectively two versions of this control, 1) ImageSize->Automatic or 2)ImageSize->NumericQ.
	If Automatic, then the setters are tightly wrapped and the overall image size is automatic.
	If NumericQ, then the imposed overall ImageSize is fixed and evenly distributed among the setters. *)


(* Following other controls like Slider, the components of the 2nd argument of Dynamic are passed through to boxes verbatim. *)
extractFunction[which_, f:Except[Temporary | {_, _} | {_, _, _}, _], ___] := extractFunction[which, {None, f, None}]
extractFunction[which_, {f_, g_}, ___] := extractFunction[which, {None, f, g}]

extractFunction[which_, Temporary, ___] := extractFunction[which, {None, Temporary, Automatic}]

extractFunction["Start", {f:Except[None | Automatic | Temporary, _], _, _}, ___] := f
extractFunction["Drag",  {_, f:Except[None | Automatic | Temporary, _], _}, ___] := f
extractFunction["End",   {_, _, f:Except[None | Automatic | Temporary, _]}, ___] := f

extractFunction["Start", {None | Temporary, _, _}, ___] := Null&
extractFunction["Drag",  {_, None | Temporary, _}, ___] := Null&
extractFunction["End",   {_, _, None | Temporary}, ___] := Null&

extractFunction["Start", {Automatic, _, _}, ___] := Function[{val, expr}, expr = val, HoldRest]
extractFunction["Drag",  {_, Automatic, _}, ___] := Function[{val, expr}, expr = val, HoldRest]
extractFunction["End",   {_, _, Automatic}, ___] := Function[{val, expr}, expr = val, HoldRest]

extractFunction[which_, other___] := extractFunction[which, {None, Automatic, None}]


ClearAll[roundedSetter, RoundedSetterBar];


Attributes[roundedSetter] = {HoldRest};


(* case: ImageSize -> Automatic so only FrameMargins are adjusted *)
roundedSetter[{i_Integer, label_, alignment_, background_, baseStyle_, fm_, tbfm_, fs_, rr_}, settingIndex_, action_] :=
Button[
	Framed[
		label,
		Alignment -> alignment,
		Background -> Dynamic[If[settingIndex === i, background, None]],
		BaseStyle -> baseStyle,
		FrameMargins -> Dynamic[{Switch[settingIndex, i, 1.5*{fm, fm}, i-1, {0.5*fm, fm}, i+1, {fm, 0.5*fm}, _, {fm, fm}], {tbfm, tbfm}}],
		FrameStyle -> fs,
		RoundingRadius -> rr],
	(settingIndex = i; action),
	Appearance -> FEPrivate`FrontEndResource["CodeFormatterExpressions", "SuppressMouseDownNinePatchAppearance"]]


(* case: ImageSize -> NumericQ so the entire control is set to a fixed image size *)
roundedSetter[{i_Integer, label_, alignment_, background_, baseStyle_, fm_, tbfm_, fs_, rr_, n_Integer, size_, halfShrinkSize_}, settingIndex_, action_] :=
Button[
	Framed[
		label,
		Alignment -> alignment,
		Background -> Dynamic[If[settingIndex === i, background, None]],
		BaseStyle -> baseStyle,
		FrameMargins ->
			Switch[i,
				1, Dynamic[{Switch[settingIndex, 1, {fm, fm},   2, {2*halfShrinkSize + fm, fm}, _, {halfShrinkSize + fm, fm}], {tbfm, tbfm}}],
				n, Dynamic[{Switch[settingIndex, n, {fm, fm}, n-1, {fm, 2*halfShrinkSize + fm}, _, {fm, halfShrinkSize + fm}], {tbfm, tbfm}}],
				_, Dynamic[{Switch[settingIndex, i, {fm, fm}, i-1, {fm, halfShrinkSize + fm}, i+1, {halfShrinkSize + 3, fm}, _, {fm, fm}], {tbfm, tbfm}}]],
		FrameStyle -> fs,
		(* :!CodeAnalysis::BeginBlock:: *)
		(* :!CodeAnalysis::Disable::DynamicImageSize:: *)
		ImageSize ->
			Switch[i,
				1, Dynamic[Switch[settingIndex, 1, size,   2, size-2*halfShrinkSize, _, size-halfShrinkSize]],
				n, Dynamic[Switch[settingIndex, n, size, n-1, size-2*halfShrinkSize, _, size-halfShrinkSize]],
				_, Dynamic[Switch[settingIndex, i, size, i-1 | i+1, size-3*halfShrinkSize, _, size-2*halfShrinkSize]]]
		(* :!CodeAnalysis::EndBlock:: *),
		RoundingRadius -> rr],
	(settingIndex = i; action),
	Appearance -> FEPrivate`FrontEndResource["CodeFormatterExpressions", "SuppressMouseDownNinePatchAppearance"]]


ClearAll[RoundedSetterBar];


aRules = <|(* These control the individual setter appearance and mouse effects *)
	"Alignment" -> Center,
	"FontColorDefault" -> GrayLevel[0.2],
	"FontColorHover" -> Red,
	"FrameMarginHorizontal" -> 7.5,
	"FrameMarginVertical" -> 0,
	"OverlapPercentage" -> 0.125,
	"RoundingRadius" -> 3,
	"SelectionBackground" -> GrayLevel[0.98],
	"SelectionFontColor" -> GrayLevel[0.2],
	"SelectionFrameStyleDefault" -> Directive[GrayLevel[0.7], AbsoluteThickness[1]],
	"SelectionFrameStyleHover" -> Directive[Red, AbsoluteThickness[1]]|>;


Options[RoundedSetterBar] = {
	Background -> White,
	BaseStyle -> "CodeFormatterText",
	FrameMargins -> Automatic,
	FrameStyle -> Automatic,
	ImageSize -> Automatic,
	RoundingRadius -> Automatic,
	AppearanceRules -> aRules};


RoundedSetterBar[Dynamic[setting_, rest___], vals:{__}, opts:OptionsPattern[]] :=
Module[
	{
		imageSize, lrFrameMargin, tbFrameMargin, background, rRadius, alignment, ar,
		mainFrameMargins, mainFrameStyle, mainFrameThickness, mainRoundingRadius,
		setBaseStyle, setSelectionFrameStyle},
	imageSize = OptionValue[ImageSize];
	ar = <|aRules, OptionValue[AppearanceRules]|>;
	(* settingIndex and hoverQ are scoped by later DynamicModule *)
	setSelectionFrameStyle[j_] :=
		With[
			{
				d = Lookup[ar, "SelectionFrameStyleDefault", Black],
				h = Lookup[ar, "SelectionFrameStyleHover", Red]},
			Dynamic[If[settingIndex === j, If[hoverQ, h, d], None]]];
	setBaseStyle[j_] := (* FontColor needs to be replaced within BaseStyle such that the mouse effect applies as we enter the ButtonBox *)
		With[
			{
				dynamicFontColor =
					With[
						{
							d = Lookup[ar, "FontColorDefault", Black],
							h = Lookup[ar, "FontColorHover", Red],
							s = Lookup[ar, "SelectionFontColor", Black]},
						Dynamic[If[settingIndex === j, s, If[CurrentValue["MouseOver"], h, d]]]]},
			If[FreeQ[OptionValue[BaseStyle], FontColor],
				Append[Flatten[{OptionValue[BaseStyle]}], FontColor -> dynamicFontColor]
				, 
				Replace[Flatten[{OptionValue[BaseStyle]}], HoldPattern[FontColor -> v_] :> FontColor -> dynamicFontColor, {1}]]];
	lrFrameMargin = Lookup[ar, "FrameMarginHorizontal", 4];
	tbFrameMargin = Lookup[ar, "FrameMarginVertical", 4];
	background = Lookup[ar, "SelectionBackground", None];
	rRadius = Lookup[ar, "RoundingRadius", None];
	alignment = Lookup[ar, "Alignment", Center];
	With[
		{
			default = Replace[OptionValue[FrameStyle], Automatic -> Directive[AbsoluteThickness[1], GrayLevel[0.8]]],
			hover = Lookup[ar, "SelectionFrameStyle", Directive[Red, AbsoluteThickness[1]]]},
		mainFrameMargins = With[{o = OptionValue[FrameMargins]}, If[!NumericQ[o], 2, o]];
		mainFrameStyle = default(*Dynamic[If[CurrentValue["MouseOver"], hover, default]]*);
		mainFrameThickness = FirstCase[{default}, AbsoluteThickness[n_] :> n, 1, Infinity];
		mainRoundingRadius = Replace[OptionValue[RoundingRadius], Automatic -> 4]];
		
	DynamicModule[
		{settingIndex, hoverQ = False, outsideUpdating = True},
		With[
			{
				n = Length[vals],                            (* number of setters *)
				b = Lookup[ar, "OverlapPercentage", 0.125],  (* percentage of selected setter to overlap with neighber, only if ImageSize is numeric *)
				indexRules = Table[If[MatchQ[vals[[i]], _Rule], First[vals[[i]]], vals[[i]]] -> i, {i, Range[Length[vals]]}]},

			DynamicWrapper[
				Framed[
					Row[Flatten @ {
						If[imageSize === Automatic, With[{m = 0.5*lrFrameMargin}, Dynamic[Switch[settingIndex, 1, Spacer[0], 0, Spacer[2*m], _, Spacer[m]]]], Nothing],
						Table[
							With[
								{
									v = If[MatchQ[vals[[i]], _Rule], First[vals[[i]]], vals[[i]]],
									l = If[MatchQ[vals[[i]], _Rule],  Last[vals[[i]]], vals[[i]]],
									s = extractFunction["Start", rest],
									d = extractFunction["Drag", rest],
									e = extractFunction["End", rest]},
								roundedSetter[
									If[NumericQ[imageSize],
										With[{is = (imageSize - 2*mainFrameMargins - 2*mainFrameThickness)/(2*b + n*(1 - 2*b))},
											{i, l, alignment, background, setBaseStyle[i], lrFrameMargin, tbFrameMargin, setSelectionFrameStyle[i], rRadius, n, is, b*is}]
										,
										{i, l, alignment, background, setBaseStyle[i], lrFrameMargin, tbFrameMargin, setSelectionFrameStyle[i], rRadius}],
									settingIndex,
									outsideUpdating = False; s[v, setting]; d[v, setting]; e[v, setting]; outsideUpdating = True]],
							{i, Range[n]}],
						If[imageSize === Automatic, With[{m = 0.5*lrFrameMargin}, Dynamic[If[settingIndex === n, Spacer[0], Spacer[m]]]], Nothing]
					}],
					Background -> OptionValue[Background],
					FrameStyle -> mainFrameStyle,
					FrameMargins -> mainFrameMargins,
					RoundingRadius -> mainRoundingRadius,
					BaseStyle -> OptionValue[BaseStyle]],
				(
					hoverQ = CurrentValue["MouseOver"];
					If[outsideUpdating, settingIndex = If[MemberQ[indexRules[[All, 1]], setting], setting /. indexRules, 0]]),
				TrackedSymbols -> {setting}]]]] // Deploy


(* ::Section::Closed:: *)
(*Newline Option Toggler*)


(* ::Subsection::Closed:: *)
(*icons*)


iconCommas =
Graphics[
	{
		Thickness[0.021528525296017224],
		{
			FaceForm[{RGBColor[0.901961, 0.901961, 0.901961], Opacity[1.]}],
			FilledCurve[{{{0, 2, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}}}, {{{2.65, 25.04}, {13.2, 25.04}, {14.488019999999999, 25.04}, {15.53, 23.99802}, {15.53, 22.71}, {15.53, 15.16}, {15.53, 13.87198}, {14.488019999999999, 12.829999999999998}, {13.2, 12.829999999999998}, {2.65, 12.829999999999998}, {1.3619759999999999, 12.829999999999998}, {0.32, 13.87198}, {0.32, 15.16}, {0.32, 22.71}, {0.32, 23.99802}, {1.3619759999999999, 25.04}, {2.65, 25.04}}}]},
		{
			Style[#, "CodeFormatterNewlineFillColor"]& @
			FilledCurve[{{{1, 4, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {0, 1, 0}}}, {{{18.759999999999998, 10.851875000000003}, {20.32, 11.393750000000002}, {21.1, 12.249687500000002}, {21.1, 13.419687500000002}, {21.05125, 13.406562500000003}, {20.9846875, 13.400000000000002}, {20.9003125, 13.400000000000002}, {20.6490625, 13.400000000000002}, {20.40625, 13.440312500000003}, {20.171875, 13.520937500000002}, {19.9375, 13.601562500000002}, {19.7303125, 13.721562500000003}, {19.5503125, 13.880937500000002}, {19.3703125, 14.040312500000002}, {19.2259375, 14.236250000000002}, {19.1171875, 14.468750000000002}, {19.010312499999998, 14.703125000000002}, {18.956875, 14.974062500000002}, {18.956875, 15.281562500000003}, {18.956875, 15.859062500000002}, {19.15375, 16.319375}, {19.5475, 16.6625}, {19.943125, 17.007500000000004}, {20.426875, 17.180000000000003}, {20.99875, 17.180000000000003}, {21.752499999999998, 17.180000000000003}, {22.3178125, 16.903437500000003}, {22.6946875, 16.3503125}, {23.0715625, 15.797187500000003}, {23.259999999999998, 15.027500000000002}, {23.259999999999998, 14.041250000000002}, {23.259999999999998, 12.873125000000002}, {22.9365625, 11.887812500000003}, {22.2896875, 11.085312500000002}, {21.6446875, 10.282812500000002}, {20.689375, 9.687500000000004}, {19.42375, 9.299375000000003}, {18.759999999999998, 10.851875000000003}}}]},
		{
			RGBColor[0.698039, 0.698039, 0.698039], Thickness[0.03229278794402584], Opacity[1.], CapForm["Butt"], JoinForm[{"Miter", 10.}],
			JoinedCurve[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}}, {{{30.919999999999998, 16.6}, {36.74, 16.6}, {36.74, 21.270000000000003}, {34.309999999999995, 21.270000000000003}}}, CurveClosed -> {0}]},
		{
			FaceForm[{RGBColor[0.698039, 0.698039, 0.698039], Opacity[1.]}],
			FilledCurve[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}}, {{{31.68, 13.98}, {27.150000000000002, 16.6}, {31.68, 19.220000000000002}, {31.68, 13.98}}}]}},
	AspectRatio -> Automatic, ImageSize -> {47., 37.}, PlotRange -> {{0., 46.449999999999996}, {0., 37.}}]


iconComments =
Graphics[
	{
		Thickness[0.021528525296017224],
		Style[#, "CodeFormatterNewlineFillColor"]& @
		{
			FilledCurve[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}}}, {{{7.1446875, 23.3315625}, {8.9615625, 26.2115625}, {6.1743749999999995, 27.44625}, {6.5090625, 28.50375}, {9.51, 27.62625}, {9.51, 30.9}, {10.77, 30.9}, {10.77, 27.62625}, {13.768125, 28.50375}, {14.102812499999999, 27.44625}, {11.315625, 26.2115625}, {13.1325, 23.3315625}, {12.19875, 22.670625}, {10.14, 25.353749999999998}, {8.08125, 22.670625}, {7.1446875, 23.3315625}}}],
			FilledCurve[{{{1, 4, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {0, 1, 0}}}, {{{4.6034375, 17.955624999999998}, {3.4446874999999997, 19.003749999999997}, {2.5306249999999997, 20.214062499999997}, {1.86125, 21.5865625}, {1.19375, 22.960937499999996}, {0.8599999999999999, 24.503124999999997}, {0.8599999999999999, 26.213124999999998}, {0.8599999999999999, 27.924999999999997}, {1.19375, 29.467187499999998}, {1.86125, 30.839687499999997}, {2.5306249999999997, 32.2140625}, {3.4446874999999997, 33.4253125}, {4.6034375, 34.473437499999996}, {6.079999999999999, 33.2978125}, {5.0112499999999995, 32.2496875}, {4.234062499999999, 31.162187499999995}, {3.7484374999999996, 30.035312499999996}, {3.2628125, 28.910312499999996}, {3.0199999999999996, 27.636249999999997}, {3.0199999999999996, 26.213124999999998}, {3.0199999999999996, 25.502499999999998}, {3.08, 24.831249999999997}, {3.2, 24.199374999999996}, {3.3200000000000003, 23.5675}, {3.502812499999999, 22.961875}, {3.7484374999999996, 22.382499999999997}, {3.9940625, 21.804999999999996}, {4.3090625000000005, 21.248124999999998}, {4.6934375, 20.711875}, {5.077812499999999, 20.175624999999997}, {5.54, 19.647812499999997}, {6.079999999999999, 19.128437499999997}, {4.6034375, 17.955624999999998}}}]},
		{
			FaceForm[{RGBColor[0.901961, 0.901961, 0.901961], Opacity[1.]}],
			FilledCurve[{{{0, 2, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}}}, {{{10.97, 15.399999999999999}, {21.52, 15.399999999999999}, {22.80802, 15.399999999999999}, {23.85, 14.358019999999996}, {23.85, 13.07}, {23.85, 5.520000000000003}, {23.85, 4.231979999999993}, {22.80802, 3.190000000000005}, {21.52, 3.190000000000005}, {10.97, 3.190000000000005}, {9.681975999999999, 3.190000000000005}, {8.639999999999999, 4.231979999999993}, {8.639999999999999, 5.520000000000003}, {8.639999999999999, 13.07}, {8.639999999999999, 14.358019999999996}, {9.681975999999999, 15.399999999999999}, {10.97, 15.399999999999999}}}]},
		{
			RGBColor[0.698039, 0.698039, 0.698039], Thickness[0.03229278794402584], Opacity[1.], CapForm["Butt"], JoinForm[{"Miter", 10.}],
			JoinedCurve[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}}, {{{31.24, 6.969999999999999}, {37.07, 6.969999999999999}, {37.07, 11.629999999999999}, {34.63, 11.629999999999999}}}, CurveClosed -> {0}]},
		{
			FaceForm[{RGBColor[0.698039, 0.698039, 0.698039], Opacity[1.]}],
			FilledCurve[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}}, {{{32.01, 4.350000000000001}, {27.479999999999997, 6.969999999999999}, {32.01, 9.59}, {32.01, 4.350000000000001}}}]},
		{
			RGBColor[0.698039, 0.698039, 0.698039], Thickness[0.03229278794402584], Opacity[1.], CapForm["Butt"], JoinForm[{"Miter", 10.}],
			JoinedCurve[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}}, {{{20.72, 25.32}, {26.54, 25.32}, {26.54, 29.990000000000002}, {24.110000000000003, 29.990000000000002}}}, CurveClosed -> {0}]},
		{
			FaceForm[{RGBColor[0.698039, 0.698039, 0.698039], Opacity[1.]}],
			FilledCurve[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}}, {{{21.479999999999997, 22.71}, {16.95, 25.32}, {21.479999999999997, 27.939999999999998}, {21.479999999999997, 22.71}}}]}},
	AspectRatio -> Automatic, ImageSize -> {47., 37.}, PlotRange -> {{0., 46.449999999999996}, {0., 37.}}]


iconLists =
Graphics[
	{
		Thickness[0.020790020790020788],
		Style[
			{FilledCurve[{{{1, 4, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {1, 3, 3}, {1, 3, 3}}}, {{{3.4, 22.9}, {3.4, 23.2}, {3.4, 23.5}, {3.4, 23.8}, {3.4, 24.1}, {3.4, 24.3}, {3.5, 24.5}, {3.5, 24.7}, {3.6, 25.}, {3.6, 25.2}, {3.6, 25.4}, {3.6, 25.7}, {3.6, 26.}, {3.6, 26.2}, {3.6, 26.3}, {3.5, 26.5}, {3.4, 26.7}, {3.3, 26.8}, {3.1, 26.9}, {2.8, 27.}, {2.6, 27.1}, {2.3, 27.2}, {2., 27.3}, {1.6, 27.3}, {1.2, 27.3}, {1.2, 28.9}, {1.7, 28.9}, {2., 28.9}, {2.3, 29.}, {2.6, 29.1}, {2.8, 29.2}, {3., 29.4}, {3.2, 29.5}, {3.3, 29.7}, {3.4, 29.8}, {3.5, 30.}, {3.5, 30.1}, {3.5, 30.3}, {3.5, 30.6}, {3.5, 30.8}, {3.5, 31.1}, {3.5, 31.3}, {3.5, 31.6}, {3.4, 31.8}, {3.4, 32.}, {3.3, 32.3}, {3.3, 32.5}, {3.3, 32.8}, {3.3, 33.}, {3.3, 33.3}, {3.3, 34.3}, {3.6, 34.9}, {4.1, 35.3}, {4.7, 35.7}, {5.5, 35.8}, {6.7, 35.8}, {8.1, 35.8}, {8.1, 34.4}, {7.4, 34.4}, {7., 34.4}, {6.7, 34.4}, {6.4, 34.3}, {6.2, 34.3}, {6., 34.2}, {5.8, 34.1}, {5.7, 34.}, {5.6, 33.9}, {5.6, 33.7}, {5.5, 33.6}, {5.5, 33.4}, {5.5, 33.1}, {5.5, 32.6}, {5.5, 32.2}, {5.5, 31.7}, {5.5, 31.2}, {5.5, 30.8}, {5.5, 30.2}, {5.5, 29.5}, {5.4, 29.}, {5.1, 28.7}, {4.9, 28.5}, {4.5, 28.3}, {3.9, 28.2}, {3.9, 28.1}, {4.5, 28.}, {4.9, 27.8}, {5.2, 27.5}, {5.4, 27.2}, {5.6, 26.7}, {5.6, 26.}, {5.6, 25.4}, {5.6, 24.9}, {5.6, 24.5}, {5.6, 24.}, {5.6, 23.6}, {5.6, 23.1}, {5.6, 22.8}, {5.6, 22.6}, {5.7, 22.5}, {5.8, 22.3}, {5.9, 22.2}, {6., 22.1}, {6.1, 22.}, {6.3, 21.9}, {6.6, 21.9}, {6.8, 21.9}, {7.2, 21.8}, {7.6, 21.8}, {8.4, 21.8}, {8.4, 20.4}, {6.8, 20.4}, {5.6, 20.4}, {4.8, 20.6}, {4.2, 21.}, {3.6, 21.2}, {3.4, 21.9}, {3.4, 22.9}}}]}, 
			FaceForm[RGBColor[0.698039, 0.698039, 0.698039, 1.]]],
		Style[
			{FilledCurve[{{{1, 4, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {0, 1, 0}}}, {{{30.6, 2.}, {32.2, 2.5}, {32.9, 3.3999999999999986}, {32.9, 4.600000000000001}, {32.9, 4.600000000000001}, {32.8, 4.600000000000001}, {32.7, 4.600000000000001}, {32.5, 4.600000000000001}, {32.2, 4.600000000000001}, {32., 4.700000000000003}, {31.8, 4.799999999999997}, {31.6, 4.899999999999999}, {31.4, 5.100000000000001}, {31.2, 5.300000000000001}, {31.1, 5.5}, {31., 5.699999999999999}, {30.9, 5.899999999999999}, {30.8, 6.199999999999999}, {30.8, 6.5}, {30.8, 7.100000000000001}, {31., 7.5}, {31.4, 7.899999999999999}, {31.8, 8.2}, {32.3, 8.399999999999999}, {32.8, 8.399999999999999}, {33.6, 8.399999999999999}, {34.1, 8.100000000000001}, {34.5, 7.600000000000001}, {34.9, 7.}, {35.1, 6.300000000000001}, {35.1, 5.300000000000001}, {35.1, 4.100000000000001}, {34.8, 3.1000000000000014}, {34.1, 2.299999999999997}, {33.5, 1.5}, {32.5, 0.8999999999999986}, {31.2, 0.5}, {30.6, 2.}}}]},
			"CodeFormatterNewlineFillColor"],
		Style[
			{FilledCurve[{{{1, 4, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {0, 1, 0}}}, {{{17.3, 2.}, {18.9, 2.5}, {19.6, 3.3999999999999986}, {19.6, 4.600000000000001}, {19.6, 4.600000000000001}, {19.5, 4.600000000000001}, {19.4, 4.600000000000001}, {19.2, 4.600000000000001}, {18.9, 4.600000000000001}, {18.7, 4.700000000000003}, {18.5, 4.799999999999997}, {18.3, 4.899999999999999}, {18.1, 5.100000000000001}, {17.9, 5.300000000000001}, {17.8, 5.5}, {17.7, 5.699999999999999}, {17.6, 5.899999999999999}, {17.5, 6.199999999999999}, {17.5, 6.5}, {17.5, 7.100000000000001}, {17.7, 7.5}, {18.1, 7.899999999999999}, {18.5, 8.2}, {19., 8.399999999999999}, {19.5, 8.399999999999999}, {20.3, 8.399999999999999}, {20.8, 8.100000000000001}, {21.2, 7.600000000000001}, {21.6, 7.}, {21.8, 6.300000000000001}, {21.8, 5.300000000000001}, {21.8, 4.100000000000001}, {21.5, 3.1000000000000014}, {20.8, 2.299999999999997}, {20.2, 1.5}, {19.2, 0.8999999999999986}, {17.9, 0.5}, {17.3, 2.}}}]},
			"CodeFormatterNewlineFillColor"],
		Style[
			{JoinedCurve[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}}, {{{39.8, 7.699999999999999}, {45.6, 7.699999999999999}, {45.6, 12.399999999999999}, {43.2, 12.399999999999999}}}, CurveClosed -> {0}]},
			CapForm["Butt"], JoinForm[{"Miter", 10.}], Thickness[0.031185031185031183], RGBColor[0.698039, 0.698039, 0.698039, 1.]],
		Style[
			{FilledCurve[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}}, {{{40.6, 5.100000000000001}, {36., 7.699999999999999}, {40.5, 10.3}, {40.5, 5.100000000000001}}}]}, 
			FaceForm[RGBColor[0.698039, 0.698039, 0.698039, 1.]]],
		Style[
			{JoinedCurve[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}}, {{{14.3, 26.1}, {20.1, 26.1}, {20.1, 30.8}, {17.7, 30.8}}}, CurveClosed -> {0}]},
			CapForm["Butt"], JoinForm[{"Miter", 10.}], Thickness[0.031185031185031183], RGBColor[0.698039, 0.698039, 0.698039, 1.]],
		Style[
			{FilledCurve[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}}, {{{15., 23.5}, {10.5, 26.1}, {15., 28.8}, {15., 23.5}}}]}, 
			FaceForm[RGBColor[0.698039, 0.698039, 0.698039, 1.]]],
		Style[
			{FilledCurve[{{{0, 2, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}}}, {{{14.3, 3.8999999999999986}, {11.9, 3.8999999999999986}, {11., 3.8999999999999986}, {10.2, 4.600000000000001}, {10.2, 5.600000000000001}, {10.2, 14.5}, {10.2, 15.399999999999999}, {10.9, 16.2}, {11.9, 16.2}, {14.3, 16.2}, {15.2, 16.2}, {16., 15.5}, {16., 14.5}, {16., 5.600000000000001}, {15.9, 4.700000000000003}, {15.2, 3.8999999999999986}, {14.3, 3.8999999999999986}}}]}, 
			"CodeFormatterNewlineFillColor"], 
		Style[
			{FilledCurve[{{{0, 2, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}}}, {{{27.2, 3.8999999999999986}, {24.8, 3.8999999999999986}, {23.9, 3.8999999999999986}, {23.1, 4.600000000000001}, {23.1, 5.600000000000001}, {23.1, 14.5}, {23.1, 15.399999999999999}, {23.8, 16.2}, {24.8, 16.2}, {27.2, 16.2}, {28.1, 16.2}, {28.9, 15.5}, {28.9, 14.5}, {28.9, 5.600000000000001}, {28.9, 4.700000000000003}, {28.2, 3.8999999999999986}, {27.2, 3.8999999999999986}}}]}, 
			"CodeFormatterNewlineFillColor"]},
	ImageSize -> {49., 37.}, PlotRange -> {{0., 48.1}, {0., 37.}}, AspectRatio -> Automatic]


iconOperators =
Graphics[
	{
		Thickness[0.021528525296017224],
		{
			FaceForm[{RGBColor[0.901961, 0.901961, 0.901961], Opacity[1.]}],
			FilledCurve[{{{0, 2, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}}}, {{{2.65, 32.04}, {13.2, 32.04}, {14.488019999999999, 32.04}, {15.53, 30.998024}, {15.53, 29.71}, {15.53, 22.16}, {15.53, 20.87198}, {14.488019999999999, 19.83}, {13.2, 19.83}, {2.65, 19.83}, {1.3619759999999999, 19.83}, {0.32, 20.87198}, {0.32, 22.16}, {0.32, 29.71}, {0.32, 30.998024}, {1.3619759999999999, 32.04}, {2.65, 32.04}}}]},
		{
			Style[#, "CodeFormatterNewlineFillColor"]& @
			FilledCurve[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}}}, {{{14.23, 8.11}, {11.17, 8.11}, {11.17, 9.91}, {14.23, 9.91}, {14.23, 13.15}, {16.21, 13.15}, {16.21, 9.91}, {19.45, 9.91}, {19.45, 8.11}, {16.21, 8.11}, {16.21, 4.69}, {14.23, 4.69}, {14.23, 8.11}}}]},
		{
			RGBColor[0.698039, 0.698039, 0.698039], Thickness[0.03229278794402584], Opacity[1.], CapForm["Butt"], JoinForm[{"Miter", 10.}],
			JoinedCurve[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}}, {{{22.919999999999998, 23.61}, {28.74, 23.61}, {28.74, 28.270000000000003}, {26.310000000000002, 28.270000000000003}}}, CurveClosed -> {0}]},
		{
			FaceForm[{RGBColor[0.698039, 0.698039, 0.698039], Opacity[1.]}],
			FilledCurve[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}}, {{{23.68, 20.99}, {19.150000000000002, 23.61}, {23.68, 26.22}, {23.68, 20.99}}}]},
		{
			RGBColor[0.698039, 0.698039, 0.698039], Thickness[0.03229278794402584], Opacity[1.], CapForm["Butt"], JoinForm[{"Miter", 10.}],
			JoinedCurve[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}}, {{{26.330000000000002, 8.25}, {32.160000000000004, 8.25}, {32.160000000000004, 12.920000000000002}, {29.72, 12.920000000000002}}}, CurveClosed -> {0}]},
		{
			FaceForm[{RGBColor[0.698039, 0.698039, 0.698039], Opacity[1.]}],
			FilledCurve[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}}, {{{27.1, 5.640000000000001}, {22.57, 8.25}, {27.1, 10.869999999999997}, {27.1, 5.640000000000001}}}]}},
	AspectRatio -> Automatic, ImageSize -> {47., 37.}, PlotRange -> {{0., 46.449999999999996}, {0., 37.}}]


iconSemicolons =
Graphics[
	{
		Thickness[0.021528525296017224],
		{
			FaceForm[{RGBColor[0.901961, 0.901961, 0.901961], Opacity[1.]}],
			FilledCurve[{{{0, 2, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}}}, {{{2.65, 25.04}, {13.2, 25.04}, {14.488019999999999, 25.04}, {15.53, 23.99802}, {15.53, 22.71}, {15.53, 15.16}, {15.53, 13.87198}, {14.488019999999999, 12.829999999999998}, {13.2, 12.829999999999998}, {2.65, 12.829999999999998}, {1.3619759999999999, 12.829999999999998}, {0.32, 13.87198}, {0.32, 15.16}, {0.32, 22.71}, {0.32, 23.99802}, {1.3619759999999999, 25.04}, {2.65, 25.04}}}]},
		{
			Style[#, "CodeFormatterNewlineFillColor"]& @
			FilledCurve[{{{1, 4, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}}, {{1, 4, 3}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {0, 1, 0}}}, {{{18.58, 20.8671875}, {18.58, 21.1915625}, {18.634375, 21.494374999999998}, {18.743125, 21.775624999999998}, {18.853749999999998, 22.058749999999996}, {19.0075, 22.3053125}, {19.204375, 22.5153125}, {19.401249999999997, 22.725312499999998}, {19.6375, 22.893124999999998}, {19.913125, 23.018749999999997}, {20.190625, 23.14625}, {20.49625, 23.21}, {20.83, 23.21}, {21.16375, 23.21}, {21.469375, 23.14625}, {21.746875, 23.018749999999997}, {22.024375, 22.893124999999998}, {22.260624999999997, 22.725312499999998}, {22.455624999999998, 22.5153125}, {22.6525, 22.3053125}, {22.8053125, 22.058749999999996}, {22.9140625, 21.775624999999998}, {23.0246875, 21.494374999999998}, {23.08, 21.1915625}, {23.08, 20.8671875}, {23.08, 20.5446875}, {23.0246875, 20.2428125}, {22.9140625, 19.9615625}, {22.8053125, 19.6803125}, {22.6525, 19.4309375}, {22.455624999999998, 19.213437499999998}, {22.260624999999997, 18.9978125}, {22.024375, 18.83}, {21.746875, 18.709999999999997}, {21.469375, 18.589999999999996}, {21.16375, 18.529999999999998}, {20.83, 18.529999999999998}, {20.49625, 18.529999999999998}, {20.190625, 18.589999999999996}, {19.913125, 18.709999999999997}, {19.6375, 18.83}, {19.401249999999997, 18.9978125}, {19.204375, 19.213437499999998}, {19.0075, 19.4309375}, {18.853749999999998, 19.6803125}, {18.743125, 19.9615625}, {18.634375, 20.2428125}, {18.58, 20.5446875}, {18.58, 20.8671875}}, {{18.7628125, 11.124687499999999}, {19.5690625, 11.3871875}, {20.1596875, 11.721874999999999}, {20.5346875, 12.128749999999998}, {20.9115625, 12.535625}, {21.1, 12.989374999999999}, {21.1, 13.489999999999998}, {20.959375, 13.489999999999998}, {20.689375, 13.489999999999998}, {20.425, 13.5359375}, {20.166249999999998, 13.6278125}, {19.9075, 13.7196875}, {19.680625, 13.853749999999998}, {19.485625, 14.03}, {19.290625, 14.208124999999999}, {19.1340625, 14.425624999999998}, {19.0159375, 14.6825}, {18.8996875, 14.939374999999998}, {18.8415625, 15.232812499999998}, {18.8415625, 15.5628125}, {18.8415625, 16.1853125}, {19.0525, 16.685}, {19.474375, 17.061875}, {19.89625, 17.440624999999997}, {20.416562499999998, 17.63}, {21.0353125, 17.63}, {21.830312499999998, 17.63}, {22.429375, 17.330937499999997}, {22.8325, 16.732812499999998}, {23.237499999999997, 16.1346875}, {23.439999999999998, 15.298437499999999}, {23.439999999999998, 14.224062499999999}, {23.439999999999998, 13.0278125}, {23.112812499999997, 12.01625}, {22.4584375, 11.189374999999998}, {21.8040625, 10.360624999999999}, {20.8215625, 9.753124999999999}, {19.5109375, 9.366874999999999}, {18.7628125, 11.124687499999999}}}]},
		{
			RGBColor[0.698039, 0.698039, 0.698039], Thickness[0.03229278794402584], Opacity[1.], CapForm["Butt"],
			JoinForm[{"Miter", 10.}], JoinedCurve[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}}, {{{30.919999999999998, 16.6}, {36.74, 16.6}, {36.74, 21.270000000000003}, {34.309999999999995, 21.270000000000003}}}, CurveClosed -> {0}]},
		{
			FaceForm[{RGBColor[0.698039, 0.698039, 0.698039], Opacity[1.]}],
			FilledCurve[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}}, {{{31.68, 13.98}, {27.150000000000002, 16.6}, {31.68, 19.220000000000002}, {31.68, 13.98}}}]}},
	AspectRatio -> Automatic, ImageSize -> {47., 37.}, PlotRange -> {{0., 46.449999999999996}, {0., 37.}}]


iconStructures =
Graphics[
	{
		Thickness[0.021528525296017224],
		Style[#, "CodeFormatterNewlineFillColor"]& @
		{
			FilledCurve[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}}}, {{{18.119999999999997, 35.79}, {23.393437499999997, 35.79}, {23.393437499999997, 34.349999999999994}, {20.1, 34.349999999999994}, {20.1, 21.749999999999996}, {23.393437499999997, 21.749999999999996}, {23.393437499999997, 20.31}, {18.119999999999997, 20.31}, {18.119999999999997, 35.79}}}],
			FilledCurve[{{{1, 4, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {0, 1, 0}}}, {{{28.08, 2.0518750000000017}, {29.64, 2.5937500000000018}, {30.42, 3.449687500000002}, {30.42, 4.619687500000001}, {30.37125, 4.606562500000002}, {30.3046875, 4.600000000000001}, {30.2203125, 4.600000000000001}, {29.9690625, 4.600000000000001}, {29.72625, 4.640312500000002}, {29.491875, 4.720937500000002}, {29.2575, 4.801562500000002}, {29.0503125, 4.921562500000002}, {28.8703125, 5.080937500000002}, {28.6903125, 5.240312500000002}, {28.5459375, 5.436250000000001}, {28.4371875, 5.668750000000002}, {28.330312499999998, 5.903125000000002}, {28.276875, 6.174062500000002}, {28.276875, 6.481562500000002}, {28.276875, 7.059062500000001}, {28.47375, 7.519375000000002}, {28.8675, 7.862500000000002}, {29.263125, 8.207500000000001}, {29.746875, 8.380000000000003}, {30.31875, 8.380000000000003}, {31.072499999999998, 8.380000000000003}, {31.6378125, 8.103437500000002}, {32.0146875, 7.550312500000002}, {32.3915625, 6.997187500000002}, {32.58, 6.227500000000002}, {32.58, 5.241250000000002}, {32.58, 4.073125000000002}, {32.2565625, 3.087812500000002}, {31.6096875, 2.2853125000000016}, {30.9646875, 1.4828125000000019}, {30.009375, 0.887500000000002}, {28.74375, 0.4993750000000019}, {28.08, 2.0518750000000017}}}]},
		{
			Style[#, "CodeFormatterNewlineFillColorSubtle"]& @
			FilledCurve[{{{0, 2, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}}}, {{{2.65, 35.59}, {13.2, 35.59}, {14.488019999999999, 35.59}, {15.53, 34.548024}, {15.53, 33.26}, {15.53, 25.71}, {15.53, 24.42198}, {14.488019999999999, 23.380000000000003}, {13.2, 23.380000000000003}, {2.65, 23.380000000000003}, {1.3619759999999999, 23.380000000000003}, {0.32, 24.42198}, {0.32, 25.71}, {0.32, 33.26}, {0.32, 34.548024}, {1.3619759999999999, 35.59}, {2.65, 35.59}}}]},
		{
			RGBColor[0.698039, 0.698039, 0.698039], Thickness[0.03229278794402584], Opacity[1.], CapForm["Butt"], JoinForm[{"Miter", 10.}],
			JoinedCurve[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}}, {{{30.919999999999998, 27.15}, {36.74, 27.15}, {36.74, 31.82}, {34.309999999999995, 31.82}}}, CurveClosed -> {0}]},
		{
			FaceForm[{RGBColor[0.698039, 0.698039, 0.698039], Opacity[1.]}],
			FilledCurve[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}}, {{{31.68, 24.53}, {27.150000000000002, 27.15}, {31.68, 29.77}, {31.68, 24.53}}}]},
		{
			FaceForm[{RGBColor[0.901961, 0.901961, 0.901961], Opacity[1.]}],
			FilledCurve[{{{0, 2, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}}}, {{{11.96, 16.23}, {22.51, 16.23}, {23.798019999999998, 16.23}, {24.84, 15.188019999999995}, {24.84, 13.899999999999999}, {24.84, 6.349999999999998}, {24.84, 5.061980000000002}, {23.798019999999998, 4.020000000000003}, {22.51, 4.020000000000003}, {11.96, 4.020000000000003}, {10.67198, 4.020000000000003}, {9.629999999999999, 5.061980000000002}, {9.629999999999999, 6.349999999999998}, {9.629999999999999, 13.899999999999999}, {9.629999999999999, 15.188019999999995}, {10.67198, 16.23}, {11.96, 16.23}}}]},
		{
			RGBColor[0.698039, 0.698039, 0.698039], Thickness[0.03229278794402584], Opacity[1.], CapForm["Butt"], JoinForm[{"Miter", 10.}],
			JoinedCurve[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}}, {{{39.230000000000004, 7.800000000000001}, {45.05, 7.800000000000001}, {45.05, 12.46}, {42.620000000000005, 12.46}}}, CurveClosed -> {0}]},
		{
			FaceForm[{RGBColor[0.698039, 0.698039, 0.698039], Opacity[1.]}],
			FilledCurve[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}}}, {{{39.99, 5.18}, {35.46, 7.800000000000001}, {39.99, 10.41}, {39.99, 5.18}}}]}},
	AspectRatio -> Automatic, ImageSize -> {47., 37.}, PlotRange -> {{0., 46.449999999999996}, {0., 37.}}]


(* ::Subsection::Closed:: *)
(*functions*)


infoButton[tooltip_] :=
Tooltip[
	DynamicModule[{hoverQ},
		DynamicWrapper[
			Graphics[
				{
					EdgeForm[], Dynamic[FaceForm[If[hoverQ, GrayLevel[.84], GrayLevel[.9]]]],
					Disk[{0, 0}, 1],
					Text[
						Style["?", FontFamily -> "Source Sans Pro", 10.5, FontWeight -> Plain, GrayLevel[.2]],
						{0, 0},
						{-.23, .15}]},
				ImageSize -> {12, 12}, ImagePadding -> {{0, 1}, {1, 0}}, BaselinePosition -> Scaled[.2]],
			hoverQ = CurrentValue["MouseOver"]]],
	tooltip,
	TooltipDelay -> 0, TooltipStyle -> {Background -> GrayLevel[.98], CellFrame -> False}
]


infoTooltipCode[type1_String, type2_String] :=
Framed[
	Style[
		tre["InfoTooltip" <> type1 <> "Code" <> type2],
		FontFamily -> Dynamic[CurrentValue[{StyleHints, "CodeFont"}]],
		LinebreakAdjustments -> {0.85, 2, 10, 0, 1},
		LineSpacing -> {1, 0},
		FontWeight -> "DemiBold"],
	ImageSize -> {Full, Automatic},
	Background -> GrayLevel[1],
	FrameStyle -> Directive[AbsoluteThickness[1], GrayLevel[0.941176]],
	RoundingRadius -> 3,
	FrameMargins -> {{5, 5}, {4, 4}}]


infoTooltip[type_String] :=
Framed[
	Column[
		{
			Pane[
				Column[
					{
						Style[tr["Break" <> type <> "Unstyled"], FontWeight -> "DemiBold"],
						tr["InfoTooltip" <> type <> "TextIntro"],
						infoTooltipCode[type, "Intro"]},
					Spacings -> {0, {0, 0, 0.5}}],
				FrameMargins -> 1.5{{4, 4}, {4, 4}}],
			Pane[
				Column[
					{
						Style[tr["ToggleAlways"], FontWeight -> "DemiBold"],
						tr["InfoTooltip" <> type <> "TextAlways"],
						infoTooltipCode[type, "Always"],
						Style[tr["ToggleNever"], FontWeight -> "DemiBold"],
						tr["InfoTooltip" <> type <> "TextNever"],
						infoTooltipCode[type, "Never"],
						Style[tr["ToggleAutomatic"], FontWeight -> "DemiBold"],
						tr["InfoTooltip" <> type <> "TextAutomatic"],
						infoTooltipCode[type, "Automatic"]},
					Spacings -> {{}, {0, {0, 0.5, 1}, 0}}],
				FrameMargins -> 1.5{{4, 4}, {4, 4}}]},
		Spacings -> {{}, {1, 1, 1}},
		Background -> {GrayLevel[0.901961], GrayLevel[0.980392]}
	],
	BaseStyle -> {"CodeFormatterText", FontSize -> 12, LineSpacing -> {1, 0}, LineIndent -> 0},
	ImageSize -> {185, All},
	FrameMargins -> 0,
	FrameStyle -> Directive[GrayLevel[0.854902], AbsoluteThickness[1]]]


optionToggler[optionName_String, Dynamic[optionValue_], title_, icon_] :=
With[{$optsPath = CodeFormatter`$optsPath},
	Row[{
		icon,
		Spacer[5],
		Grid[
			{
				{
					Style[tr["Break" <> title], FontSize -> 12.5, FontColor -> GrayLevel[.2]],
					infoButton[infoTooltip[title]]},
				{
					Item[#, Alignment -> Left]& @
					RoundedSetterBar[
						Dynamic[optionValue,
							Function[
								optionValue = #;
								If[$VersionNumber >= 12.2,
									CurrentValue[$FrontEnd, {$optsPath, optionName}] = #;
									CurrentValue[$FrontEnd, {$optsPath, "FormatMethod"}] = "NewlineRules";
									CodeFormatter`Notebooks`formatSelectedCell[]]]],
						{False -> tr["ToggleNever"], Automatic -> tr["ToggleAutomatic"], True -> tr["ToggleAlways"]},
						AppearanceRules -> <|
							"FontColorHover" -> Dynamic[FontColor /. CurrentValue[{StyleDefinitions, "CodeFormatterNewlineColor"}]],
							"SelectionFrameStyleHover" -> Directive[AbsoluteThickness[1], "CodeFormatterNewlineColor"],
							"FrameMarginHorizontal" -> 3|>],
					SpanFromLeft}},
			Spacings -> {0, .2}, Alignment -> {{Left, Right}},
			BaseStyle -> "CodeFormatterText"]
	}]
]


(* ::Section::Closed:: *)
(*Airiness + Newline pods*)


(* ::Subsection::Closed:: *)
(*Newline Toggler Pod*)


NewlineTogglerPod[
	Dynamic[parameters:{semicolons_, operators_, groups_, commas_, ctrlStruct_, scopingStruct_, comments_}]
] :=
With[
	{
		$optsPath = CodeFormatter`$optsPath,
		optionTogglerList =
			Pane[Row[{#, Spacer[5]}]& @
				Column[
					Riffle[
						{
							optionToggler["NewlinesBetweenSemicolons", Dynamic[semicolons],    "Semicolons",        iconSemicolons],
							optionToggler["NewlinesBetweenOperators",  Dynamic[operators],     "Operators",         iconOperators],
							optionToggler["NewlinesInGroups",          Dynamic[groups],        "ListsAssociations", iconLists],
							optionToggler["NewlinesBetweenCommas",     Dynamic[commas],        "Commas",            iconCommas],
							optionToggler["NewlinesInControl",         Dynamic[ctrlStruct],    "ControlStructures", iconStructures],
							optionToggler["NewlinesInScoping",         Dynamic[scopingStruct], "ScopingStructures", iconStructures],
							optionToggler["NewlinesInComments",        Dynamic[comments],      "Comments",          iconComments]},
						delimiter2]],
				ImageSize -> {All, 201}, Scrollbars -> {False, True}, AppearanceElements -> {}]
	},

	EventHandler[
		Framed[
			optionTogglerList,
			Background -> GrayLevel[.99],
			FrameMargins -> {{4, 0}, {4, 4}},
			FrameStyle -> GrayLevel[.7],
			ImageSize -> Automatic,
			Alignment -> Center,
			RoundingRadius -> 4],
		{
			"MouseDown" :> (
				(* This is an important line. In order for the newline options to behave as expected, Airiness must be set to Automatic. *)
				CurrentValue[$FrontEnd, {$optsPath, "Airiness"}] = Automatic)},
		PassEventsDown -> True
	]
]


(* ::Subsection::Closed:: *)
(*Airiness + Newline*)


advancedOpener[Dynamic[isAdvanced_]] :=
	EventHandler[
		Pane[
			Grid[{{
				Opener[Dynamic[isAdvanced]],
				Style[Dynamic[FEPrivate`FrontEndResource["CodeFormatterStrings", "LinebreakLabel"]], "CodeFormatterText"]}}],
			ImageSize -> dialogSuggestedWidth],
		"MouseClicked" :> (isAdvanced = !isAdvanced)]


AirinessControl[Dynamic[airinessValue_, rest___], Dynamic[isAdvanced_], Dynamic[parameters_]] :=
Column[
	{
		Style[tr["AirinessLabel"], "CodeFormatterText"],
		Item[AirinessSlider[Dynamic[airinessValue, rest], Dynamic[parameters]], Alignment -> Center],
		PaneSelector[
			{
				False -> advancedOpener[Dynamic[isAdvanced]],
				True ->
					Column[{
						advancedOpener[Dynamic[isAdvanced]],
						NewlineTogglerPod[Dynamic[parameters]]}]},
			Dynamic[isAdvanced],
			ImageSize -> Automatic]},
	BaseStyle -> "CodeFormatterText"]


(* ::Section::Closed:: *)
(*Preset Controls*)


(* ::Subsection::Closed:: *)
(*Utilities*)


airinessKeys = {
	"NewlinesBetweenSemicolons",
	"NewlinesBetweenOperators",
	"NewlinesInGroups",
	"NewlinesBetweenCommas",
	"NewlinesInControl",
	"NewlinesInScoping",
	"NewlinesInComments"};


indentKeys = {
	"InteractiveIndentationCharacter",
	"InteractiveTabWidth"};


With[{$optsPath = CodeFormatter`$optsPath, airinessKeys = airinessKeys, sliderPresets = CodeFormatter`optionValues},

getUniqueAirinessValues[] :=
Select[KeyTake[AbsoluteCurrentValue[$FrontEnd, {$optsPath}, <||>], airinessKeys], # =!= Automatic&];

presetIsEqualToCurrentSettingsQ[presetName_String] :=
Module[{baseValues, presetValues, currentSettings},
	baseValues = AssociationThread[airinessKeys -> Automatic];
	presetValues = KeySort @ <|baseValues, Replace[AbsoluteCurrentValue[$FrontEnd, {$optsPath, "Presets", presetName}], (Inherited | $Failed) -> <||>]|>;
	currentSettings =
		KeySort @
			Join[
				KeyTake[<|baseValues, AbsoluteCurrentValue[$FrontEnd, {$optsPath}, <||>]|>, airinessKeys],
				KeyTake[AbsoluteCurrentValue[$FrontEnd, {$optsPath}, <||>], indentKeys],
				<|"FormatMethod" -> Replace[AbsoluteCurrentValue[$FrontEnd, {$optsPath, "FormatMethod"}], (Inherited | $Failed) -> <||>]|>];
	presetValues === currentSettings];

presetIsEqualToCurrentSettingsQ[presetName_] := False;

sliderPositionIsEqualToCurrentSettingsQ[sliderValue_] :=
Module[{currentSettings},
	currentSettings = Values[KeyTake[AbsoluteCurrentValue[$FrontEnd, {$optsPath}, <||>], airinessKeys]];
	Replace[FirstPosition[CodeFormatter`optionValues, currentSettings, {Key[None]}], {Key[n_]} :> n] === sliderValue]

]


(* ::Subsection::Closed:: *)
(*Icons*)


deleteIcon[Dynamic[edgeCol_]] :=
Graphics[
	{
		Thickness[0.067], 
		{
			Dynamic[edgeCol], CapForm["Round"], JoinForm[{"Miter", 10.}], 
			JoinedCurve[{{{1, 4, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}}}, {{{14.427999999999999, 7.463000000000021}, {14.427999999999999, 3.616890000000021}, {11.31011, 0.49900000000002187}, {7.4639999999999995, 0.49900000000002187}, {3.617889, 0.49900000000002187}, {0.5, 3.616890000000021}, {0.5, 7.463000000000021}, {0.5, 11.309111000000021}, {3.617889, 14.427000000000021}, {7.4639999999999995, 14.427000000000021}, {11.31011, 14.427000000000021}, {14.427999999999999, 11.309111000000021}, {14.427999999999999, 7.463000000000021}}}, CurveClosed -> {0}]},
		{
			FaceForm[{GrayLevel[.18]}],
			Line[{{5, 5}, {10, 10}}],
			Line[{{5, 10}, {10, 5}}]}},
	AspectRatio -> Automatic, ImageSize -> {17., 17.}, PlotRange -> {{0., 14.927}, {0., 14.927}}, ImagePadding -> 1]


penIcon[Dynamic[edgeCol_]] :=
Graphics[
	{
		Thickness[0.067], 
		{
			Dynamic[edgeCol], CapForm["Round"], JoinForm[{"Miter", 10.}], 
			JoinedCurve[{{{1, 4, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}}}, {{{14.427999999999999, 7.463000000000021}, {14.427999999999999, 3.616890000000021}, {11.31011, 0.49900000000002187}, {7.4639999999999995, 0.49900000000002187}, {3.617889, 0.49900000000002187}, {0.5, 3.616890000000021}, {0.5, 7.463000000000021}, {0.5, 11.309111000000021}, {3.617889, 14.427000000000021}, {7.4639999999999995, 14.427000000000021}, {11.31011, 14.427000000000021}, {14.427999999999999, 11.309111000000021}, {14.427999999999999, 7.463000000000021}}}, CurveClosed -> {0}]},
		{
			FaceForm[{GrayLevel[.18]}],
			FilledCurve[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {1, 3, 3}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {1, 3, 3}}}, {{{8.78699999999992, 11.15300000000002}, {5.610999999999876, 5.65300000000002}, {5.348999999999933, 3.3430000000000177}, {7.218999999999824, 4.7239999999999895}, {10.394999999999982, 10.22399999999999}, {10.48070000000007, 10.37190000000004}, {10.503999999999905, 10.547900000000027}, {10.459799999999973, 10.712999999999965}, {10.415499999999952, 10.878100000000018}, {10.307199999999852, 11.018799999999942}, {10.158999999999878, 11.103999999999985}, {9.667999999999893, 11.388000000000034}, {9.520099999999957, 11.473900000000015}, {9.343999999999937, 11.497299999999996}, {9.17879999999991, 11.453300000000013}, {9.013499999999908, 11.409199999999998}, {8.872499999999945, 11.301099999999963}, {8.78699999999992, 11.15300000000002}}}]}},
	AspectRatio -> Automatic, ImageSize -> {17., 17.}, PlotRange -> {{0., 14.927}, {0., 14.927}}, ImagePadding -> 1]


(* ::Subsection::Closed:: *)
(*Save As*)


SaveAsPresetInterfaceBody[] :=
With[{$optsPath = CodeFormatter`$optsPath},
	DynamicModule[{},
		InputField[
			Dynamic[CurrentValue[EvaluationNotebook[], {TaggingRules, "PresetName"}],
				Function[
					CurrentValue[EvaluationNotebook[], {TaggingRules, "PresetName"}] = #;
					CurrentValue[EvaluationNotebook[], {TaggingRules, "NameAlreadyExists"}] =
						With[{a = AbsoluteCurrentValue[$FrontEnd, {$optsPath, "Presets"}]},
							AssociationQ[a] && KeyExistsQ[a, CurrentValue[EvaluationNotebook[], {TaggingRules, "PresetName"}]]]]],
			String,
			ImageSize -> Full,
			BoxID :> "name",
			ContinuousAction -> True,
			FieldHint -> FEPrivate`FrontEndResource["CodeFormatterStrings", "PresetsNameHint"],
			FieldHintStyle -> {"InputTextPlaceholder"}],
		SynchronousInitialization -> False,
		Initialization :> (FrontEnd`MoveCursorToInputField[EvaluationNotebook[], "name"])]
]


SaveAsPresetInterfaceFooter[Dynamic[activePresetKey_]] :=
With[{$optsPath = CodeFormatter`$optsPath},
	Grid[
		{{
			PaneSelector[
				{
					True -> tr["PresetsNameTaken"],
					False -> ""},
				Dynamic[CurrentValue[EvaluationNotebook[], {TaggingRules, "NameAlreadyExists"}]],
				ImageSize -> Automatic, BaseStyle -> {"InputError"}],
			ChoiceButtons[
				{(* appearances *)
					roundedRectButtonAppearance[
						Dynamic[FEPrivate`FrontEndResourceString["okButtonText"]],
						FEPrivate`Switch[FEPrivate`$ProductIDName, "WolframAlphaNB", "Orange1", "WolframFinancePlatform", "Blue1", _, "Red1"],
						Dynamic[
							Not[
								Or[
									CurrentValue[EvaluationNotebook[], {TaggingRules, "PresetName"}] == "",
									CurrentValue[EvaluationNotebook[], {TaggingRules, "NameAlreadyExists"}]]]],
						ImageSize -> {{38, Full}, 25},
						ImageMargins -> {{Inherited, Inherited}, {3, 3}}],
					roundedRectButtonAppearance[
						Dynamic[FEPrivate`FrontEndResourceString["cancelButtonText"]],
						"Gray1",
						Dynamic[True],
						ImageSize -> {{38, Full}, 25},
						ImageMargins -> {{Inherited, Inherited}, {3, 3}}]},
				{(* actions *)
					(
						CurrentValue[$FrontEnd, {$optsPath, "Presets", CurrentValue[EvaluationNotebook[], {TaggingRules, "PresetName"}]}] =
							<|
								getUniqueAirinessValues[],
								"FormatMethod" -> CurrentValue[$FrontEnd, {$optsPath, "FormatMethod"}],
								"InteractiveIndentationCharacter" -> CurrentValue[$FrontEnd, {$optsPath, "InteractiveIndentationCharacter"}],
								"InteractiveTabWidth" -> CurrentValue[$FrontEnd, {$optsPath, "InteractiveTabWidth"}]|>;
						CurrentValue[$FrontEnd, {$optsPath, "LastPresetUsed"}] = CurrentValue[EvaluationNotebook[], {TaggingRules, "PresetName"}];
						activePresetKey = CurrentValue[EvaluationNotebook[], {TaggingRules, "PresetName"}];
						NotebookDelete[EvaluationCell[]];
						DialogReturn[])
					,
					DialogReturn[]},
				{(* options *)
					{
						ImageSize -> Automatic, (*ignore DefaultButtonSize*)
						Appearance -> FrontEndResource["CodeFormatterExpressions", "SuppressMouseDownNinePatchAppearance"],
						Enabled ->
							Dynamic[
								Not[
									Or[
										CurrentValue[EvaluationNotebook[], {TaggingRules, "PresetName"}] == "",
										CurrentValue[EvaluationNotebook[], {TaggingRules, "NameAlreadyExists"}]]]]}
					,
					{
						ImageSize -> Automatic, (*ignore DefaultButtonSize*)
						Appearance -> FrontEndResource["CodeFormatterExpressions", "SuppressMouseDownNinePatchAppearance"],
						Enabled -> True}}]
			}},
		Alignment -> {{Left, Right}},
		ItemSize -> {{Automatic, Fit}}]
]


SaveAsPresetButton[Dynamic[activePresetKey_]] :=
With[
	{
		body = SaveAsPresetInterfaceBody[],
		footer = SaveAsPresetInterfaceFooter[Dynamic[activePresetKey]],
		width = dialogSuggestedWidth,
		$optsPath = CodeFormatter`$optsPath
	},

	roundedRectButton[
		CreateDialog[
			{
				Cell[FrontEndResource["CodeFormatterStrings", "PresetsSaveAs"], "DialogHeader"],
				Cell["", "DialogDelimiter"],
				Cell[BoxData[ToBoxes[body]], "DialogBody"],
				Cell["", "DialogDelimiter"],
				Cell[BoxData[ToBoxes[footer]], "DialogFooter", CellMargins -> {Inherited, {0, 0}}]},
			Initialization :> Replace[AbsoluteCurrentValue[$FrontEnd, {$optsPath, "Presets"}], Except[_Association] -> <||>],
			TaggingRules -> {"PresetName" -> ""},
			WindowSize ->
				{
					300,
					(* 416665: There is a fundamental drawing issue on Mac if WindowSize is All. Fix the height on Mac as a workaround. *)
					If[$OperatingSystem === "MacOSX", 188, All]},
			Modal -> True,
			Background -> Dynamic[CurrentValue[{"SystemColors", "Window"}]],
			StyleDefinitions -> "Dialog.nb"],
		tr["PresetsSaveAs"],
		"Gray2",
		{	
			ImageSize -> Automatic,
			Enabled -> True,
			Method -> "Queued",
			Appearance -> FEPrivate`FrontEndResource["CodeFormatterExpressions", "SuppressMouseDownNinePatchAppearance"]},
		{ImageSize -> {Automatic, 25}}]
]


(* ::Subsection::Closed:: *)
(*Update*)


UpdatePresetButton[Dynamic[presetName_]] :=
With[{$optsPath = CodeFormatter`$optsPath},
	roundedRectButton[ 
		CurrentValue[$FrontEnd, {$optsPath, "Presets", presetName}] =
			<|
				getUniqueAirinessValues[],
				"FormatMethod" -> CurrentValue[$FrontEnd, {$optsPath, "FormatMethod"}],
				"InteractiveIndentationCharacter" -> CurrentValue[$FrontEnd, {$optsPath, "InteractiveIndentationCharacter"}],
				"InteractiveTabWidth" -> CurrentValue[$FrontEnd, {$optsPath, "InteractiveTabWidth"}]|>,
		tr["PresetsUpdate"],
		"Gray2",
		{
			Enabled -> Dynamic[StringQ[presetName] && !presetIsEqualToCurrentSettingsQ[presetName]],
			Appearance -> FEPrivate`FrontEndResource["CodeFormatterExpressions", "SuppressMouseDownNinePatchAppearance"]},
		{ImageSize -> {Automatic, 25}}]
]


(* ::Subsection::Closed:: *)
(*Presets viewer*)


(* ::Subsubsection::Closed:: *)
(*Preset list controls*)


SetAttributes[
	{
		editPresetInterfaceBody, editPresetInterfaceFooter, editPresetNameButton,
		deletePresetNameButton, presetBubbleDisplayMode,
		PresetBubbleList},
	{HoldAll}]


editPresetInterfaceBody[currentName_] :=
With[{$optsPath = CodeFormatter`$optsPath},
	DynamicModule[{},
		InputField[
			Dynamic[CurrentValue[EvaluationNotebook[], {TaggingRules, "PresetName"}],
				Function[
					CurrentValue[EvaluationNotebook[], {TaggingRules, "PresetName"}] = #;
					CurrentValue[EvaluationNotebook[], {TaggingRules, "NameAlreadyExists"}] =
						With[{a = AbsoluteCurrentValue[$FrontEnd, {$optsPath, "Presets"}]},
							And[
								CurrentValue[EvaluationNotebook[], {TaggingRules, "PresetName"}] != currentName,
								AssociationQ[a],
								KeyExistsQ[a, CurrentValue[EvaluationNotebook[], {TaggingRules, "PresetName"}]]]]]],
			String,
			ImageSize -> Full,
			BoxID :> "name",
			ContinuousAction -> True,
			FieldHint -> FEPrivate`FrontEndResource["CodeFormatterStrings", "PresetsNameHint"],
			FieldHintStyle -> {"InputTextPlaceholder"}],
		SynchronousInitialization -> False,
		Initialization :> (
			CurrentValue[EvaluationNotebook[], {TaggingRules, "PresetName"}] = currentName;
			FrontEnd`MoveCursorToInputField[EvaluationNotebook[], "name"])]
]


editPresetInterfaceFooter[currentName_, activePresetKey_, presetList_] :=
With[{$optsPath = CodeFormatter`$optsPath},
	Grid[
		{{
			PaneSelector[
				{
					True -> tr["PresetsNameTaken"],
					False -> ""},
				Dynamic[CurrentValue[EvaluationNotebook[], {TaggingRules, "NameAlreadyExists"}]],
				ImageSize -> Automatic, BaseStyle -> {"InputError"}],
			ChoiceButtons[
				{(* appearances *)
					roundedRectButtonAppearance[
						Dynamic[FEPrivate`FrontEndResourceString["okButtonText"]],
						FEPrivate`Switch[FEPrivate`$ProductIDName, "WolframAlphaNB", "Orange1", "WolframFinancePlatform", "Blue1", _, "Red1"],
						Dynamic[
							Or[
								CurrentValue[EvaluationNotebook[], {TaggingRules, "PresetName"}] == currentName, (* allow no change to occur *)
								Not[
									Or[
										CurrentValue[EvaluationNotebook[], {TaggingRules, "PresetName"}] == "",
										CurrentValue[EvaluationNotebook[], {TaggingRules, "NameAlreadyExists"}]]]]],
						ImageSize -> {{38, Full}, 25},
						(* KMD: not sure why margins are off in the footer cell, so zero out top/bottom CellMargins. This leaves 12pts of space, so add 3 *)
						ImageMargins -> {{Inherited, Inherited}, {3, 3}}], 
					roundedRectButtonAppearance[
						Dynamic[FEPrivate`FrontEndResourceString["cancelButtonText"]],
						"Gray1",
						Dynamic[True],
						ImageSize -> {{38, Full}, 25},
						ImageMargins -> {{Inherited, Inherited}, {3, 3}}]},
				{(* actions *)
					(
						If[CurrentValue[EvaluationNotebook[], {TaggingRules, "PresetName"}] == currentName,
							DialogReturn[] (* no-op if name hasn't changed *)
							,
							Module[{values, currentSettings},
								values = AbsoluteCurrentValue[$FrontEnd, {$optsPath, "Presets", currentName}];
								currentSettings = AbsoluteCurrentValue[$FrontEnd, {$optsPath, "Presets"}];
								CurrentValue[$FrontEnd, {$optsPath, "Presets"}] =
									<|
										KeyDrop[currentSettings, currentName],
										CurrentValue[EvaluationNotebook[], {TaggingRules, "PresetName"}] -> values|>];
							(* only update the active preset key if it was the one that was edited *)
							If[activePresetKey == currentName,
								activePresetKey = CurrentValue[EvaluationNotebook[], {TaggingRules, "PresetName"}]
							];
							presetList =
								Pane[
									Column[presetBubbleFunction /@ Keys[AbsoluteCurrentValue[$FrontEnd, {$optsPath, "Presets"}, <||>]]],
									Scrollbars -> {False, Automatic},
									AppearanceElements -> {}];
							DialogReturn[]
						])
					,
					DialogReturn[]},
				{(* options *)
					{
						ImageSize -> Automatic, (*ignore DefaultButtonSize*)
						Appearance -> FrontEndResource["CodeFormatterExpressions", "SuppressMouseDownNinePatchAppearance"],
						Enabled ->
							Dynamic[
								Or[
									CurrentValue[EvaluationNotebook[], {TaggingRules, "PresetName"}] == currentName, (* allow no change to occur *)
									Not[
										Or[
											CurrentValue[EvaluationNotebook[], {TaggingRules, "PresetName"}] == "",
											CurrentValue[EvaluationNotebook[], {TaggingRules, "NameAlreadyExists"}]]]]]}
					,
					{
						ImageSize -> Automatic, (*ignore DefaultButtonSize*)
						Appearance -> FrontEndResource["CodeFormatterExpressions", "SuppressMouseDownNinePatchAppearance"],
						Enabled -> True}}]
			}},
		Alignment -> {{Left, Right}},
		ItemSize -> {{Automatic, Fit}}]
]


editPresetNameButton[presetList_, activePresetKey_, displayName_, mouseOver_] :=
With[
	{
		body = editPresetInterfaceBody[displayName],
		footer = editPresetInterfaceFooter[displayName, activePresetKey, presetList]
	},
	DynamicWrapper[
		Tooltip[
			EventHandler[
				Mouseover[
					penIcon[Dynamic[GrayLevel[0.7]]],
					penIcon[Dynamic[FontColor /. CurrentValue[{StyleDefinitions, "CodeFormatterHighlightColor"}]]]],
				"MouseClicked" :> (
					CreateDialog[
						{
							Cell[FrontEndResource["CodeFormatterStrings", "PresetsEdit"], "DialogHeader"],
							Cell["", "DialogDelimiter"],
							Cell[BoxData[ToBoxes[body]], "DialogBody"],
							Cell["", "DialogDelimiter"],
							Cell[BoxData[ToBoxes[footer]], "DialogFooter", CellMargins -> {Inherited, {0, 0}}]},
						TaggingRules -> {"PresetName" -> ""},
						WindowSize -> {300, All},
						Modal -> True,
						Background -> Dynamic[CurrentValue[{"SystemColors", "Window"}]],
						StyleDefinitions -> "Dialog.nb"]),
				Method -> "Queued",
				PassEventsDown -> False],
			tr["PresetsEdit"]],
		mouseOver = CurrentValue["MouseOver"]]
]


deletePresetNameButton[presetList_, activePresetKey_, displayName_, mouseOver_] :=
With[{$optsPath = CodeFormatter`$optsPath},
	DynamicWrapper[
		Tooltip[
			EventHandler[
				Mouseover[
					deleteIcon[Dynamic[GrayLevel[0.7]]],
					deleteIcon[Dynamic[FontColor /. CurrentValue[{StyleDefinitions, "CodeFormatterHighlightColor"}]]]],
				"MouseClicked" :> (
					CurrentValue[$FrontEnd, {$optsPath, "Presets"}] = KeyDrop[AbsoluteCurrentValue[$FrontEnd, {$optsPath, "Presets"}], displayName];
					If[activePresetKey == displayName, activePresetKey = None];
					If[CurrentValue[$FrontEnd, {$optsPath, "Presets"}] === <||>,
						NotebookDelete[EvaluationCell[]]
						,
						presetList =
							Pane[
								Column[presetBubbleFunction /@ Keys[AbsoluteCurrentValue[$FrontEnd, {$optsPath, "Presets"}, <||>]]],
								Scrollbars -> {False, Automatic},
								AppearanceElements -> {}]
					]),
				Method -> "Queued",
				PassEventsDown -> False],
			tr["PresetsRemove"]],
		mouseOver = CurrentValue["MouseOver"]]
]


presetBubbleDisplayMode[presetList_, activePresetKey_, displayName_, mouseOverEditButton_, mouseOverDeleteButton_] :=
Grid[
	{{
		Dynamic[displayName],
		Grid[
			{{
				editPresetNameButton[presetList, activePresetKey, displayName, mouseOverEditButton],
				deletePresetNameButton[presetList, activePresetKey, displayName, mouseOverDeleteButton]}}]}},
	Alignment -> {{Left, Right}},
	ItemSize -> {{Automatic, Fit}}]


SetAttributes[{PresetBubble}, {HoldRest}];


With[{$optsPath = CodeFormatter`$optsPath, airinessKeys = airinessKeys, indentKeys = indentKeys},

PresetBubble[presetName_(*String*), presetList_, activePresetKey_, parameters_, indentationParameters_] :=
buttonAppearance[(* gives rounded rect appearance *)
	DynamicModule[{displayName = presetName, mouseOverEditButton = False, mouseOverDeleteButton = False},
		EventHandler[
			Style[
				presetBubbleDisplayMode[presetList, activePresetKey, displayName, mouseOverEditButton, mouseOverDeleteButton],
				"CodeFormatterText", FontWeight -> "DemiBold"],
			"MouseClicked" :> (
				If[Not[Or[mouseOverEditButton, mouseOverDeleteButton]],
					Module[{presetValues, currentSettings},
						presetValues = AbsoluteCurrentValue[$FrontEnd, {$optsPath, "Presets", displayName}, <||>];
						currentSettings = AbsoluteCurrentValue[$FrontEnd, {$optsPath}, <||>];
						CurrentValue[$FrontEnd, {$optsPath}] =
							<|
								currentSettings,
								Automatic& /@ KeyTake[currentSettings, airinessKeys],
								KeyTake[presetValues, airinessKeys]|>;
						(* Indentation settings were an early feature and are stored in a different FrontEnd setting *)
						CurrentValue[$FrontEnd, {$optsPath, indentKeys[[1]]}] = indentationParameters[[1]];
						CurrentValue[$FrontEnd, {$optsPath, indentKeys[[2]]}] = indentationParameters[[2]];
						CurrentValue[$FrontEnd, {$optsPath, "LastPresetUsed"}] = displayName;
					];
					activePresetKey = displayName;
					parameters = Values[KeyTake[AbsoluteCurrentValue[$FrontEnd, {$optsPath}], airinessKeys]];
					indentationParameters = Values[KeyTake[AbsoluteCurrentValue[$FrontEnd, {$optsPath}], indentKeys]];
					NotebookDelete[EvaluationCell[]]]),
			PassEventsDown -> True]],
	{All, All}]

]


(*
from kevind:
PresetBubbleList (HoldAll) needs to be evaluated so that it can be fully defined and self-contained within the preset popup menu's button action.
PresetBubble (HoldRest) also needs to be evaluated so its function is fully defined and self-contained within the DynamicModule's Initialization option.
presetBubbleFunction is resolved at build-time (evaluation of the DynamicModule) and properly localizes the DM variable 'presetList'.
*)
PresetBubbleList[activePresetKey_, parameters_, indentationParameters_] :=
With[{$optsPath = CodeFormatter`$optsPath},
	DynamicModule[{presetList, presetBubbleFunction},
		presetBubbleFunction = Quiet @ Function[Evaluate[PresetBubble[#, presetList, activePresetKey, parameters, indentationParameters]]];
		Dynamic[presetList],
		Initialization :> (
			presetList =
				Pane[
					Column[presetBubbleFunction /@ Keys[AbsoluteCurrentValue[$FrontEnd, {$optsPath, "Presets"}, <||>]]],
					Scrollbars -> {False, Automatic},
					AppearanceElements -> {}])]
]


(* ::Subsubsection::Closed:: *)
(*Select Preset popup*)


presetPopupMenuAppearance[Dynamic[activePresetKey_]] :=
With[{$optsPath = CodeFormatter`$optsPath},
	Framed[
		Grid[
			{{
				Dynamic[
					Which[
						AbsoluteCurrentValue[$FrontEnd, {$optsPath, "Presets"}, <||>] === <||>,
							Dynamic[FEPrivate`FrontEndResource["CodeFormatterStrings", "PresetsPopupLabel"]],
						activePresetKey === None,
							Dynamic[FEPrivate`FrontEndResource["CodeFormatterStrings", "PresetsPopupLabel"]],
						True,
							Row[{If[presetIsEqualToCurrentSettingsQ[activePresetKey], "", "* "], activePresetKey}]
					],
					TrackedSymbols :> {activePresetKey}
				],
				Chevron[Dynamic[Down]]}},
			BaseStyle -> "CodeFormatterText",
			Alignment -> {{Left, Right}},
			ItemSize -> {{Automatic, Fit}}],
		Background -> White,
		FrameMargins -> {5{1, 1}, 4{1, 1}},
		ImageSize -> {dialogSuggestedWidth, Automatic}, 
		RoundingRadius -> 3,
		FrameStyle -> Dynamic @ If[CurrentValue["MouseOver"],
			Directive["CodeFormatterHighlightColor", AbsoluteThickness[1]],
			Directive[GrayLevel[.7], AbsoluteThickness[1]]]]
]


SelectPresetPopup[Dynamic[activePresetKey_], Dynamic[parameters_], Dynamic[indentationParameters_]] :=
With[
	{
		$optsPath = CodeFormatter`$optsPath,
		presetBubbleList = PresetBubbleList[activePresetKey, parameters, indentationParameters],
		width = dialogSuggestedWidth
	},
	Button[
		presetPopupMenuAppearance[Dynamic[activePresetKey]],
		(* Paclet 1.5 targets 12.1+. System`AttachCell did not exist until 12.2. Use the legacy call signature. *)
		MathLink`CallFrontEnd[FrontEnd`AttachCell[
			EvaluationBox[],
			Cell[BoxData[ToBoxes[
				Panel[
					If[Length[AbsoluteCurrentValue[$FrontEnd, {$optsPath, "Presets"}, <||>]] == 0,
						Dynamic[FEPrivate`FrontEndResource["CodeFormatterStrings", "PresetsEmpty"]]
						,
						presetBubbleList],
					FrameMargins -> 4,
					ImageSize -> width,
					Appearance -> {
						"Default" -> FrontEnd`ToFileName[{"Misc"}, "popupRightBottom.9.png"],
						"Hover" -> FrontEnd`ToFileName[{"Misc"}, "popupRightBottom.9.png"],
						"Pressed" -> FrontEnd`ToFileName[{"Misc"}, "popupRightBottom.9.png"]}]]]],
			{0, {Right, Top}},
			{Right, Bottom},
			"ClosingActions" -> {"OutsideMouseClick"}]],
		ImageSize -> All,
		Appearance -> FEPrivate`FrontEndResource["CodeFormatterExpressions", "SuppressMouseDownNinePatchAppearance"]]
]


(* ::Subsection::Closed:: *)
(*Main Preset controls*)


PresetControls[
	Dynamic[activePresetKey_],
	Dynamic[parameters:{semicolons_, operators_, groups_, commas_, ctrlStruct_, scopingStruct_, comments_}],
	Dynamic[indentationParameters:{indentationCharacter_, tabWidth_}]
] :=
Pane[
	Grid[
		{
			{
				Style[tr["PresetsLabel"], "CodeFormatterText"],
				SpanFromLeft},
			{
				Style[
					SelectPresetPopup[Dynamic[activePresetKey], Dynamic[parameters], Dynamic[indentationParameters]],
					"CodeFormatterText"],
				SpanFromLeft},
			{
				UpdatePresetButton[Dynamic[activePresetKey]],
				"",
				SaveAsPresetButton[Dynamic[activePresetKey]]}},
		Alignment -> {{Left, Center, Right}},
		ItemSize -> {{Automatic, Fit, Automatic}}],
	ImageSize -> {dialogSuggestedWidth, Automatic}]


(* ::Section::Closed:: *)
(*Indentation Menu*)


IndentationMenu[Dynamic[indentation_], iFunc_Function, Dynamic[width_], wFunc_Function, formatCell_Function] :=
ActionMenu[
	Framed[
		Grid[
			{{
				Dynamic[
					Which[
						indentation === "tab", Dynamic[FEPrivate`FrontEndResource["CodeFormatterStrings", "TabMenuItem"]],
						width === "2", Dynamic[FEPrivate`FrontEndResource["CodeFormatterStrings", "Space2MenuItem"]],
						width === "4", Dynamic[FEPrivate`FrontEndResource["CodeFormatterStrings", "Space4MenuItem"]],
						width === "6", Dynamic[FEPrivate`FrontEndResource["CodeFormatterStrings", "Space6MenuItem"]],
						width === "8", Dynamic[FEPrivate`FrontEndResource["CodeFormatterStrings", "Space8MenuItem"]],
						True, ""
					]
				],
				"",
				Chevron[Dynamic[Down]]}},
			ItemSize -> {{Automatic, Fit, Automatic}},
			Spacings -> 0,
			Alignment -> {{Left, Center, Right}, Baseline},
			BaseStyle -> "CodeFormatterText"
		],
		
		ImageSize -> {dialogSuggestedWidth/2, Automatic},
		Background -> White,
		FrameMargins -> {5{1, 1}, 4{1, 1}},
		RoundingRadius -> 3,
		FrameStyle -> Dynamic @ If[CurrentValue["MouseOver"],
			Directive["CodeFormatterHighlightColor", AbsoluteThickness[1]],
			Directive[GrayLevel[.7], AbsoluteThickness[1]]]
	],
		
	{
		tr["TabMenuItem"] :> (indentation = "tab"; iFunc[indentation]),
		Delimiter,
		tr["Space2MenuItem"] :> (indentation = "space"; width = "2"; iFunc[indentation]; wFunc[width]; formatCell[]),
		tr["Space4MenuItem"] :> (indentation = "space"; width = "4"; iFunc[indentation]; wFunc[width]; formatCell[]),
		tr["Space6MenuItem"] :> (indentation = "space"; width = "6"; iFunc[indentation]; wFunc[width]; formatCell[]),
		tr["Space8MenuItem"] :> (indentation = "space"; width = "8"; iFunc[indentation]; wFunc[width]; formatCell[])
	},
	Appearance -> None
]	


(* ::Section::Closed:: *)
(*Package Footer*)


EndPackage[];
