(* ::Package:: *)

(* ::Section::Closed:: *)
(*Package Header*)


Unprotect["CodeFormatterCell`*"];


ClearAll["CodeFormatterCell`*"];


BeginPackage["CodeFormatterCell`"];


If[!MemberQ[$ContextPath, "CodeFormatter`Generate`UIElements`"],	
	Get[ToFileName[{DirectoryName[NotebookFileName[]]}, "UIElements.wl"]]
];


(* ::Section::Closed:: *)
(*Code Formatter Docked Cell*)


toCell[contents_] := Cell[BoxData @ ToBoxes[contents]]


formatter =
DynamicModule[{},
	Grid[
		{{
			Dynamic[FEPrivate`FrontEndResource["CodeFormatterStrings", "AirinessLabel"]],
			Spacer[10],
			DynamicModule[{semicolons, operators, groups, commas, ctrlStruct, scopingStruct, comments},
				DynamicWrapper[(* DynamicWrapper is part of fix 402825 *)
					#,
					{semicolons, operators, groups, commas, ctrlStruct, scopingStruct, comments, CodeFormatter`$InteractiveAiriness} =
						Lookup[
							CurrentValue[$FrontEnd, {CodeAssistOptions, "CodeToolsOptions", "CodeFormat"}],
							{
								"NewlinesBetweenSemicolons", "NewlinesBetweenOperators", "NewlinesInGroups", "NewlinesBetweenCommas", "NewlinesInControl",
								"NewlinesInScoping", "NewlinesInComments", "Airiness"},
							Automatic],
					TrackedSymbols :> {} (* only trigger if the CurrentValue changes *)
				]& @
				AirinessSlider[
					Dynamic[
						CodeFormatter`$InteractiveAiriness, 
						{
							Automatic,
							(* this fires on slider mouse up but after internal side effect of setting "CodeFormat" CurrentValue key-values *)
							Function[{val, expr},
								expr = val;
								If[$VersionNumber >= 12.2, CodeFormatter`Notebooks`formatSelectedCell[]],
								HoldAll
							]
						}
					],
					Dynamic[{semicolons, operators, groups, commas, ctrlStruct, scopingStruct, comments}]
				],
				Initialization :> (
					(* Populate the local option value variables. *)
					With[{optVals = Replace[CurrentValue[$FrontEnd, {CodeAssistOptions, "CodeToolsOptions", "CodeFormat"}], Except[_Association] -> <||>]},
						semicolons = Lookup[optVals, "NewlinesBetweenSemicolons", Automatic];
						operators = Lookup[optVals, "NewlinesBetweenOperators", Automatic];
						groups = Lookup[optVals, "NewlinesInGroups", Automatic];
						commas = Lookup[optVals, "NewlinesBetweenCommas", Automatic];
						ctrlStruct = Lookup[optVals, "NewlinesInControl", Automatic];
						scopingStruct = Lookup[optVals, "NewlinesInScoping", Automatic];
						comments = Lookup[optVals, "NewlinesInComments", Automatic]];
				)
			],

			delimiter,
			
			Style[tr["IndentationLabel"], "CodeFormatterText"],
			Spacer[10],
			DynamicWrapper[(* DynamicWrapper is part of fix 402825 *)
				#,
				{CodeFormatter`$InteractiveIndentationCharacter, CodeFormatter`$InteractiveTabWidth} =
					Lookup[
						CurrentValue[$FrontEnd, {CodeAssistOptions, "CodeToolsOptions", "CodeFormat"}],
						{"InteractiveIndentationCharacter", "InteractiveTabWidth"},
						Automatic],
				TrackedSymbols :> {} (* only trigger if the CurrentValue changes *)
			]& @
			IndentationMenu[
				Dynamic[CodeFormatter`$InteractiveIndentationCharacter],
				Function[{val}, If[$VersionNumber >= 12.2, CurrentValue[$FrontEnd, {CodeAssistOptions, "CodeToolsOptions", "CodeFormat", "InteractiveIndentationCharacter"}] = val]],
				Dynamic[CodeFormatter`$InteractiveTabWidth],
				Function[{val}, If[$VersionNumber >= 12.2, CurrentValue[$FrontEnd, {CodeAssistOptions, "CodeToolsOptions", "CodeFormat", "InteractiveTabWidth"}] = val]],
				Function[If[$VersionNumber >= 12.2, CodeFormatter`Notebooks`formatSelectedCell[]]]
			]
		}},
		Spacings -> {0, 0}, Alignment -> {Center, Center}, BaseStyle -> "CodeFormatterText"
	],
	
	Initialization :> (
		Module[{opts},

			Needs["CodeFormatter`Notebooks`"];

			If[$VersionNumber >= 12.2,
				
				CurrentValue[$FrontEnd, {CodeAssistOptions, "CodeToolsOptions", "CodeFormat", "InteractiveReparse"}] = True;
				opts = Replace[CurrentValue[$FrontEnd, {CodeAssistOptions, "CodeToolsOptions", "CodeFormat"}], Except[_Association] -> <||>];

				CodeFormatter`$InteractiveAiriness = Lookup[opts, "Airiness", 0];
				CodeFormatter`$InteractiveTabWidth = Lookup[opts, "InteractiveTabWidth", "4"];
				CodeFormatter`$InteractiveIndentationCharacter = Lookup[opts, "InteractiveIndentationCharacter", "space"];
				CodeFormatter`$InteractiveReparse = Lookup[opts, "InteractiveReparse", True];
			];
		];
		
		(* Make sure the open/close button in the Toolbar package is displayed *)
		CurrentValue[$FrontEnd, {PrivateFrontEndOptions, "InterfaceSettings", "CodeFormatter", "ShowDropDown"}] = True;
		
	)
];


(* ::Section::Closed:: *)
(*Output*)


codeFormatter = FileNameJoin[{ParentDirectory[NotebookDirectory[]], "FrontEnd", "TextResources", "CodeFormatter.tr"}];


(* Ensure WIReS resources are visible *)
ResourceFunction["AddResourceSystem", ResourceSystemBase -> "https://www.internalcloud.wolfram.com/obj/resourcesystem/api/1.0"]["WIReS"];


ResourceFunction["WriteTextResource"][codeFormatter, "@@resource CodeFormatterLocalizedExpressions English", "PackageToolbarPreferencesCell" -> toCell[formatter]]


(* ::Section::Closed:: *)
(*Package Footer*)


EndPackage[];
