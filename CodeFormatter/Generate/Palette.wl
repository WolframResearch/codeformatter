(* ::Package:: *)

BeginPackage["CodeFormatter`Generate`Palette`"]

Begin["`Private`"]

Needs["CodeFormatter`Generate`GenerateSources`"]

Needs["CodeFormatter`Generate`UIElements`"]


generatePalette[] := 
Module[{nb},
  
  UsingFrontEnd[

    nb = With[{AccentColor = RGBColor[0.27,0.69,0.96]},
		CreatePalette[
			Highlighted[
				DynamicModule[{},
	
					Column[{
						FormatButton[Dynamic[AccentColor], {183, 25}],
		
						AirinessSlider[
							Dynamic[AccentColor],
			
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
							{{-1, -0.85, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 0.85, 1}},
							"Column"
						],
		
						IndentationMenu[
							Dynamic[AccentColor],
			
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
							],
							"Column"
						]
		
					}, ItemSize -> {0, 0}, Spacings -> 2, Alignment -> Left],
	
					Initialization :> 
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
						]
				],
				Background -> BackgroundCol, RoundingRadius -> 0, FrameMargins -> {{15, 15}, {20, 10}}
			]
      ,
      (*
      WindowTitle->Dynamic[FEPrivate`FrontEndResource["CodeFormatterStrings", "PaletteTitle"]]

      Cannot use FrontEndResource for WindowTitle because $Failed will appear in FE Palettes Menu

      Related bugs: xxx
      *)
      WindowTitle->"Code Formatting", Background -> BackgroundCol
    ]
];

    NotebookSave[nb, FileNameJoin[{generatedWLDir, "CodeFormatter.nb"}]]
  ]
]

Print["UsingFrontEnd... \[WatchIcon]"]

generatePalette[]

Print["Done Palette"]

End[]

EndPackage[]
