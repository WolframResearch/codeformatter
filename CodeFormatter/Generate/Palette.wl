(* ::Package:: *)

If[!MemberQ[$Path, #], PrependTo[$Path, #]]&[DirectoryName[$InputFileName, 3]]

BeginPackage["CodeFormatter`Generate`Palette`"]

Begin["`Private`"]

(*
Do not allow PacletManager to participate in finding `Generate` files

PacletManager will find e.g. CodeParser/Kernel/TokenEnum.wl when asked to find CodeParser`Generate`TokenEnum`

related issues: PACMAN-54
*)
Block[{Internal`PacletFindFile = Null&},
Needs["CodeFormatter`Generate`UIElements`"];
Needs["CodeTools`Generate`GenerateSources`"];
]


generatePalette[] := 
Module[{nb, res},
	
	Print["UsingFrontEnd... \[WatchIcon]"];

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

		Related bugs: 401490
		*)
		WindowTitle->"Code Formatting", Background -> BackgroundCol
	]
];

	If[!MatchQ[nb, _NotebookObject],
		Print["CreatePalette failed: ", nb];
		Quit[1]
	];
	
	Print["saving CodeFormatter.nb"];
	res = NotebookSave[nb, FileNameJoin[{generatedWLDir, "FrontEnd", "Palettes", "CodeFormatter.nb"}]];

	Print[res];

	If[res =!= Null,
		Quit[1]
	];
	];

	Print["Done UsingFrontEnd"];
]

generate[] := (

Print["Generating Palette..."];

generatePalette[];

Print["Done Palette"]
)

If[!StringQ[script],
  Quit[1]
]
If[AbsoluteFileName[script] === AbsoluteFileName[$InputFileName],
generate[]
]

End[]

EndPackage[]
