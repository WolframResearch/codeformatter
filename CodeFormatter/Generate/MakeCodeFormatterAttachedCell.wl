(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


Unprotect["CodeFormatterCell`*"];


ClearAll["CodeFormatterCell`*"];


BeginPackage["CodeFormatterCell`"];


If[!MemberQ[$ContextPath, "CodeFormatter`Generate`UIElements`"],	
	Get[ToFileName[{DirectoryName[NotebookFileName[]]}, "UIElements.wl"]]
];


(* ::Section:: *)
(*Code Formatter Docked Cell*)


controlRow[{controls__}, opts___] := DynamicModule[{},
	Grid[{Riffle[{controls}, delimiter]}, GridOpts],
	opts
]


toCell[contents_] := Cell[BoxData @ ToBoxes[contents]]


formatter = With[{BackgroundCol = BackgroundCol}, controlRow[
	{
		AirinessSlider[
			Dynamic[CurrentValue[EvaluationNotebook[], {TaggingRules, "CodeFormatter", "ToolbarAccentColor"}, RGBColor["#2497b7"]]],
			
			Dynamic[
				CodeFormatter`$InteractiveAiriness, 
				{
					Function[{val, expr},
						expr = val;
						If[$VersionNumber >= 12.2,
							CurrentValue[$FrontEnd, {CodeAssistOptions, "CodeToolsOptions", "CodeFormatterInteractiveAiriness"}] = val
						],
						HoldAll
					],
					Function[{val, expr}, expr = val;
						If[$VersionNumber >= 12.2, CodeFormatter`Notebooks`formatSelectedCell[]],
						HoldAll
					]
				}
			],
			{{-1, -0.85, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 0.85, 1}}
		],
					
		IndentationMenu[
			Dynamic[CurrentValue[EvaluationNotebook[], {TaggingRules, "CodeFormatter", "ToolbarAccentColor"}, RGBColor["#2497b7"]]],
			
			Dynamic[
				CodeFormatter`$InteractiveIndentationCharacter,
				Function[{val},
					If[$VersionNumber >= 12.2,
						CurrentValue[$FrontEnd, {CodeAssistOptions, "CodeToolsOptions", "CodeFormatterInteractiveIndentationCharacter"}] = val;
					]
				]
			], 
			
			Dynamic[
				CodeFormatter`$InteractiveTabWidth,
				Function[{val},
					If[$VersionNumber >= 12.2,
						CurrentValue[$FrontEnd, {CodeAssistOptions, "CodeToolsOptions", "CodeFormatterInteractiveTabWidth"}] = val;
					]
				],
				Function[{val},
					If[$VersionNumber >= 12.2, CodeFormatter`Notebooks`formatSelectedCell[]],
					HoldAll
				]
			]
		]
	},
	
	Initialization :> (
		Module[{opts},

			Needs["CodeFormatter`Notebooks`"];

			If[$VersionNumber >= 12.2,
				
				CurrentValue[$FrontEnd, {CodeAssistOptions, "CodeToolsOptions", "CodeFormatterInteractiveReparse"}] = True;
				opts = CurrentValue[$FrontEnd, {CodeAssistOptions, "CodeToolsOptions"}];

				If[FailureQ[opts],
					opts = <||>
				];

				CodeFormatter`$InteractiveAiriness = Lookup[opts, "CodeFormatterInteractiveAiriness", 0];
				CodeFormatter`$InteractiveTabWidth = Lookup[opts, "CodeFormatterInteractiveTabWidth", "4"];
				CodeFormatter`$InteractiveIndentationCharacter = Lookup[opts, "CodeFormatterInteractiveIndentationCharacter", "space"];
				CodeFormatter`$InteractiveReparse = Lookup[opts, "CodeFormatterInteractiveReparse", True];
			];
		];
		
		(* Make sure the open/close button in the Toolbar package is displayed *)
		CurrentValue[$FrontEnd, {PrivateFrontEndOptions, "InterfaceSettings", "CodeFormatter", "ShowDropDown"}] = True;
		
		(* Set options for the appearance of the parent cell *)
		SetOptions[EvaluationCell[],
			{
				CellMargins->{{0, 0}, {0, 0}},
				CellFrameMargins->{{15, 0}, {4, 4}},
				CellFrame->Dynamic[If[
					Dynamic[CurrentValue[EvaluationNotebook[], {TaggingRules, "CodeFormatter", "ToolbarState"}, False]],
					{{0,0},{2,0}}, None, None
				]],
				CellFrameColor -> Dynamic[CurrentValue[EvaluationNotebook[], {TaggingRules, "CodeFormatter", "ToolbarAccentColor"}, RGBColor["#2497b7"]]],
				Background -> BackgroundCol,
				FontSize->13,
				FontColor->Black, 
				FontWeight->Plain,
				Magnification -> 1, 
				Deployed -> True,
				Evaluator -> "System",
				DynamicUpdating -> True,
				DynamicEvaluationTimeout -> 12
			}
		]
		)
]];


(* ::Section:: *)
(*Output*)


copyCellExprs[cells_] := 
	With[{nb = NotebookPut[Notebook[Flatten[{cells}]]]},
		SelectionMove[nb, All, Notebook];
		FrontEndExecute[{FrontEndToken[nb, "CopySpecial", "CellExpression"]}]
	]


(* ::Text:: *)
(*Evaluate the following to generate the cell and put the corresponding cell expression(s) on the clipboard. Paste the DynamicModuleBox wrapped in Cell[BoxData[XXXX]] into CodeFormatter.tr*)


(* ::Input:: *)
(*toCell[formatter]//copyCellExprs*)


(* ::Section::Closed:: *)
(*Package Footer*)


EndPackage[];
