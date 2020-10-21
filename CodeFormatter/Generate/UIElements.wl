(* ::Package:: *)

(* ::Section::Closed:: *)
(*Package Header*)


Unprotect["CodeFormatter`Generate`UIElements`*"];


ClearAll["CodeFormatter`Generate`UIElements`*"];


BeginPackage["CodeFormatter`Generate`UIElements`"];


AirinessSlider::usage = "AirinessSlider[Dynamic[col], Dynamic[x], {{spec}}] displays a slider with equally-spaced ticks. {{spec}} are the explicit values x can take.
AirinessSlider[Dynamic[x], {{spec}}, width] specifies the width of the slider.
col specifies the color of the control.";

IndentationMenu::usage = "IndentationMenu[Dynamic[col], Dynamic[char], Dynamic[width]] displays a radio button bar with choices of Tab and Space, and a popup menu to choose the number of spaces.
col specifies the color of the control.";

ReparseCheckBox::usage = "ReparseCheckBox[Dynamic[col], Dynamic[x]] displays a checkbox.
col specifies the color of the control.";


(* ::Section::Closed:: *)
(*Appearance*)


BackgroundCol = GrayLevel[.95];


baseStyle = BaseStyle -> {FontFamily -> "SourceSansPro", FontSize -> 13,
		FontWeight -> Plain, FontSlant -> Plain, FontColor -> GrayLevel[0], Background -> None
	};


GridOpts = Sequence[
	ItemSize->{0, 0}, Spacings -> 0, Alignment -> Center, baseStyle
];


Chevron[Dynamic[col_], Dynamic[direction_]] := Graphics[
	{
		AbsoluteThickness[2], JoinForm["Round"], CapForm["Round"], Dynamic[col],
		Dynamic @ Line @ Switch[direction,
			Left, {{.5, -1}, {-.5, 0}, {.5, 1}},
			Right, {{-.5, -1}, {.5, 0}, {-.5, 1}},
			Up, {{-1, -.5}, {0, .5}, {1, -.5}},
			_, {{-1, .5}, {0, -.5}, {1, .5}}
		]
	},
	ImageSize->{10, 10}, BaselinePosition -> Bottom
]


delimiter = Graphics[
	{
		AbsoluteThickness[1], GrayLevel[.6], CapForm["Round"],
		Line[{{0, 0}, {0, 1}}]
	},
	ImageSize -> {32, 18}, AspectRatio -> Full
];


opener[label_, content_, Dynamic[col_]] := 
With[{grid = Grid[{{##}}, ItemSize -> {0, 0}, Spacings -> 0]&},
	DynamicModule[{openQ = False},
		grid[
			Button[
				grid[label, Spacer[3], Chevron[Dynamic[col], Dynamic[Switch[openQ, True, Left, False, Right]]]],
				openQ = !openQ, Appearance -> None
			],
			PaneSelector[{True -> grid[Spacer[8], content], False -> Spacer[{0, 0}]}, Dynamic[openQ], ImageSize -> Automatic]
		],
		Evaluate[baseStyle]
	]
]


RowOrColumn[rowOrColumn_?(MatchQ["Row"|"Column"])] := Switch[
	rowOrColumn,
	"Row", Grid[{#}, GridOpts],
	"Column", Column[#, ItemSize -> {0, 0}, Spacings -> .7, baseStyle]
]&


(* ::Section::Closed:: *)
(*Format Button*)


FormatButton[Dynamic[highlightCol_], size_:{Full, 25}] := Button[

	Highlighted[
		Dynamic@FEPrivate`FrontEndResource["CodeFormatterStrings", "FormatButton"],
		RoundingRadius -> 3, Frame -> True, ImageSize -> size, Alignment -> Center, Background -> White,
		FrameStyle -> Dynamic @ If[CurrentValue["MouseOver"],
			Directive[highlightCol, AbsoluteThickness[1]],
			Directive[GrayLevel[.7], AbsoluteThickness[1]]
		],
		BaseStyle -> {FontFamily -> "SourceSansPro", FontSize -> 12,
			FontWeight -> Plain, FontSlant -> Plain, FontColor -> GrayLevel[0], Background -> None}
	],
	
	If[$VersionNumber >= 12.2, CodeFormatter`Notebooks`formatSelectedCell[]],
	
	Appearance -> None
]


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


With[{BackgroundCol = BackgroundCol},

AirinessSlider[Dynamic[HighlightCol_], s:Dynamic[x_, ___], {{spec__}}, rowOrColumn_:"Row", size_Integer:130] :=
With[
	{
		sliderCol = GrayLevel[.75],
		(* equally space the values of spec along the slider *)
		xMap = Append[
			MapThread[Rule[#1, #2]&, {{spec}, Array[Identity, Length[{spec}], (1 - 13/size){-1, 1}]}],
			Rule[_, 0]
		]
	},
	
	DynamicModule[{hoverQ},
	RowOrColumn[rowOrColumn] @ {
			Row[{Dynamic@FEPrivate`FrontEndResource["CodeFormatterStrings", "AirinessLabel"], Spacer[{10, 15}]}],
			
			Grid[{{
			DenseIcon[Darker[sliderCol, .3]],
			Spacer[5],
			
			(* slider *)	
			DynamicWrapper[
			Overlay[
				{
					Style[
						Slider[s, {{spec}}, ContinuousAction -> True, ImageSize -> {size - 4, 15}],
						(* ensure slider handle location parity across systems *)
						ControlsRendering -> "Generic"
					],
		
					Graphics[
						{
							(* draw the slider line *)
							{sliderCol, CapForm["Round"], AbsoluteThickness[2],
							Line[{{-1, 0}, {1, 0}}]},
							
							(* draw the slider ticks *)
							FaceForm[sliderCol], EdgeForm[],
							Disk[{#, 0}, Offset[2.5]]& /@ xMap[[All, 2]],
							
							(* draw the slider ticks *)
							FaceForm[White],
							Dynamic @ If[hoverQ,
								EdgeForm[{AbsoluteThickness[1], HighlightCol}],
								EdgeForm[{AbsoluteThickness[.5], GrayLevel[.6]}]
							],
							Disk[{Dynamic[Replace[x, xMap]], 0}, Offset[6]]
				
						},
						ImageSize -> {size, 15}, AspectRatio -> Full,
						PlotRange -> {{-1, 1}, {-1, 1}}, ImagePadding -> {2{1, 1}, {0, 0}},
						Background -> BackgroundCol
					]
				},
				{1, 2}, 1,
				Alignment -> Center, ImageSize -> {size, 15}
			],
			hoverQ = CurrentValue["MouseOver"]
			],
			
			Spacer[5],
			AiryIcon[Darker[sliderCol, .3]]
			}}, GridOpts]
		}
	]
]

]


(* ::Section::Closed:: *)
(*Indentation Menu*)


IndentationMenu[Dynamic[HighlightCol_], Dynamic[indentation_, fi___], Dynamic[width_, fw___], rowOrColumn_:"Row"] :=
	RowOrColumn[rowOrColumn] @ {
		Row[{Dynamic@FEPrivate`FrontEndResource["CodeFormatterStrings", "IndentationLabel"], Spacer[{10, 15}]}],
		
		ActionMenu[
			Highlighted[
				Grid[
					{{
						Highlighted[
							Dynamic[Which[
								indentation === "tab", Dynamic@FEPrivate`FrontEndResource["CodeFormatterStrings", "TabMenuItem"],
								width === "2", Dynamic@FEPrivate`FrontEndResource["CodeFormatterStrings", "Space2MenuItem"],
								width === "4", Dynamic@FEPrivate`FrontEndResource["CodeFormatterStrings", "Space4MenuItem"],
								width === "6", Dynamic@FEPrivate`FrontEndResource["CodeFormatterStrings", "Space6MenuItem"],
								width === "8", Dynamic@FEPrivate`FrontEndResource["CodeFormatterStrings", "Space8MenuItem"],
								True, ""
							]],
							Background -> None, ImageSize -> {63, 17}, FrameMargins -> None
						],
						Chevron[Dynamic[HighlightCol], Dynamic[Down]]
					}},
					ItemSize -> 0, Spacings -> 0, Alignment -> Baseline,
					BaseStyle -> {FontFamily -> "SourceSansPro", FontSize -> 12,
						FontWeight -> Plain, FontSlant -> Plain, FontColor -> GrayLevel[0], Background -> None
					}
				],
				
				Background -> White, FrameMargins -> {5{1, 1}, {1, 1}},
				Frame -> True, RoundingRadius -> 3,
				FrameStyle -> Dynamic @ If[CurrentValue["MouseOver"],
					Directive[HighlightCol, AbsoluteThickness[1]],
					Directive[GrayLevel[.7], AbsoluteThickness[1]]
				]
			],
				
			{
				Dynamic@FEPrivate`FrontEndResource["CodeFormatterStrings", "TabMenuItem"] :>
					(indentation = "tab"; Through[Flatten[{fi}][indentation]]; Through[Flatten[{fw}][width]]),
				Delimiter,
				Dynamic@FEPrivate`FrontEndResource["CodeFormatterStrings", "Space2MenuItem"] :>
					(indentation = "space"; width = "2"; Through[Flatten[{fi}][indentation]]; Through[Flatten[{fw}][width]]),
				Dynamic@FEPrivate`FrontEndResource["CodeFormatterStrings", "Space4MenuItem"] :>
					(indentation = "space"; width = "4"; Through[Flatten[{fi}][indentation]]; Through[Flatten[{fw}][width]]),
				Dynamic@FEPrivate`FrontEndResource["CodeFormatterStrings", "Space6MenuItem"] :>
					(indentation = "space"; width = "6"; Through[Flatten[{fi}][indentation]]; Through[Flatten[{fw}][width]]),
				Dynamic@FEPrivate`FrontEndResource["CodeFormatterStrings", "Space8MenuItem"] :>
					(indentation = "space"; width = "8"; Through[Flatten[{fi}][indentation]]; Through[Flatten[{fw}][width]])
			},
			Appearance->None
		]
	}


(* ::Section::Closed:: *)
(*Reparse Checkbox*)


ReparseCheckBox[Dynamic[HighlightCol_], s:Dynamic[x_, ___]] :=
Tooltip[
	Grid[
		{{
			Row[{Dynamic@FEPrivate`FrontEndResource["CodeFormatterStrings", "ReparseLabel"], Spacer[{12, 15}]}],
			DynamicModule[{hoverQ},
			DynamicWrapper[
			Overlay[
				{
					Style[Checkbox[s], ControlsRendering -> "Generic"],
					Highlighted["", RoundingRadius -> 0, ImageSize->{20,20}, Background -> BackgroundCol],
					Highlighted[
						Dynamic @ If[x,
							Graphics[Text[Style["\[Checkmark]", FontFamily -> "SourceCodePro", Black, 14, FontWeight -> Plain, FontSlant -> Plain], {0, 0}, {.15, .1}], PlotRange -> 1],
							"", ""
						],
						Background -> White, Frame -> True, RoundingRadius -> 3,
						FrameMargins -> 0, ImageSize -> {17, 17},
						FrameStyle -> Dynamic @ If[
							hoverQ,
							Directive[AbsoluteThickness[1], HighlightCol],
							Directive[AbsoluteThickness[1], GrayLevel[.7]]
						]
					]
				},
				{1, 2, 3}, 1,
				Alignment -> Center
			],
			hoverQ = CurrentValue["MouseOver"]
			]
			]
		}},
		GridOpts
	],
	Dynamic@FEPrivate`FrontEndResource["CodeFormatterStrings", "ReparseToolTip"],
	TooltipDelay -> 0
]


(* ::Section::Closed:: *)
(*Package Footer*)


EndPackage[];
