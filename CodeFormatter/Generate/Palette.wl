(* ::Package:: *)

If[!MemberQ[$Path, #], PrependTo[$Path, #]]&[DirectoryName[$InputFileName, 3]]

BeginPackage["CodeFormatter`Generate`Palette`"]

Begin["`Private`"]

(*
auto-load any symbols that require the PacletManager that may appear later

AppearanceRules appears in CodeFormatter`Generate`UIElements`

this prevents e.g. "Get::noopen: Cannot open Forms`." messages when building

related bugs: 415177
*)
AppearanceRules
(*
Do not allow PacletManager to participate in finding `Generate` files

PacletManager will find e.g. CodeParser/Kernel/TokenEnum.wl when asked to find CodeParser`Generate`TokenEnum`

related issues: PACMAN-54
*)
Block[{Internal`PacletFindFile = Null&},
Needs["CodeFormatter`Generate`UIElements`"];
Needs["CodeTools`Generate`GenerateSources`"];
]


checkBuildDir[]


generatePalette[] := 
Module[{nb, res},
  
  Print["UsingFrontEnd... \[WatchIcon]"];

  UsingFrontEnd[

  nb =
    CreatePalette[ Deploy @
      Highlighted[
        DynamicModule[{semicolons, operators, groups, commas, ctrlStruct, scopingStruct, comments, activePresetKey},
          Column[{
            FormatButton[{dialogSuggestedWidth, 25}],

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
            AirinessControl[(* includes AirinessSlider and newline controls hidden behind an opener *)
              Dynamic[
                CodeFormatter`$InteractiveAiriness, 
                {
                  Automatic,
                  (* this fires on slider mouse up but after internal side effect of setting "CodeFormat" CurrentValue key-values *)
                  Function[{val, expr},
                    expr = val;
                    If[$VersionNumber >= 12.2, CodeFormatter`Notebooks`formatSelectedCell[]],
                    HoldAll]}],
              Dynamic[CurrentValue[$FrontEnd, {PrivateFrontEndOptions, "InterfaceSettings", "CodeFormatter", "ShowLinebreakRules"}, False]],
              Dynamic[{semicolons, operators, groups, commas, ctrlStruct, scopingStruct, comments}]],

            Style[tr["IndentationLabel"], "CodeFormatterText"],

            (* This needs a Spacings override so it's not too far from the IndentationLabel *)
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
            ],

            PresetControls[
              Dynamic[activePresetKey],
              Dynamic[{semicolons, operators, groups, commas, ctrlStruct, scopingStruct, comments}],
              Dynamic[{CodeFormatter`$InteractiveIndentationCharacter, CodeFormatter`$InteractiveTabWidth}]
            ]
          }, ItemSize -> {0, 0}, Spacings -> {{}, {{{2}}, 4 -> 0.5}}, Alignment -> Left],
  
          SaveDefinitions -> True,

          Initialization :> 
            Module[{opts},

              Needs["CodeFormatter`Notebooks`"];

              If[$VersionNumber >= 12.2,
        
                CurrentValue[$FrontEnd, {CodeAssistOptions, "CodeToolsOptions", "CodeFormat", "InteractiveReparse"}] = True;
                opts = Replace[CurrentValue[$FrontEnd, {CodeAssistOptions, "CodeToolsOptions", "CodeFormat"}], Except[_Association] -> <||>];

                (* fill the options with those from the last used preset if it exists; override values from the previous session *)
                activePresetKey = AbsoluteCurrentValue[$FrontEnd, {CodeAssistOptions, "CodeToolsOptions", "CodeFormat", "LastPresetUsed"}, None];
                If[KeyExistsQ[AbsoluteCurrentValue[$FrontEnd, {CodeAssistOptions, "CodeToolsOptions", "CodeFormat", "Presets"}, <||>], activePresetKey],
                  opts = <|opts, AbsoluteCurrentValue[$FrontEnd, {CodeAssistOptions, "CodeToolsOptions", "CodeFormat", "Presets", activePresetKey}]|>
                ];
                (* "FormatMethod" is saved in a preset so don't redefine it if not needed *)
                CurrentValue[$FrontEnd, {CodeAssistOptions, "CodeToolsOptions", "CodeFormat", "FormatMethod"}] = Lookup[opts, "FormatMethod", "AirinessSlider"];
                
                (* If a preset exists then these symbols take the existing preset values, or the indicated default if no preset or preset value exists *)
                CodeFormatter`$InteractiveAiriness = Lookup[opts, "Airiness", 0];
                CodeFormatter`$InteractiveTabWidth = Lookup[opts, "InteractiveTabWidth", "4"];
                CodeFormatter`$InteractiveIndentationCharacter = Lookup[opts, "InteractiveIndentationCharacter", "space"];
                CodeFormatter`$InteractiveReparse = Lookup[opts, "InteractiveReparse", True];

                semicolons = Lookup[opts, "NewlinesBetweenSemicolons", Automatic];
                operators = Lookup[opts, "NewlinesBetweenOperators", Automatic];
                groups = Lookup[opts, "NewlinesInGroups", Automatic];
                commas = Lookup[opts, "NewlinesBetweenCommas", Automatic];
                ctrlStruct = Lookup[opts, "NewlinesInControl", Automatic];
                scopingStruct = Lookup[opts, "NewlinesInScoping", Automatic];
                comments = Lookup[opts, "NewlinesInComments", Automatic];

                (* Corner case: saving a preset relies on CurrentValues being accurate, so set them at initialization to the current state of the interface *)
                CurrentValue[$FrontEnd, {CodeAssistOptions, "CodeToolsOptions", "CodeFormat"}] = <|
                  (* don't blow away existing presets or other non-interface values *)
                  CurrentValue[$FrontEnd, {CodeAssistOptions, "CodeToolsOptions", "CodeFormat"}],
                  (* overwrite relevant values with current state of interface *)
                  "InteractiveIndentationCharacter" -> CodeFormatter`$InteractiveIndentationCharacter,
                  "InteractiveTabWidth" -> CodeFormatter`$InteractiveTabWidth,
                  "Airiness" -> CodeFormatter`$InteractiveAiriness,
                  "NewlinesBetweenSemicolons" -> semicolons,
                  "NewlinesBetweenOperators" -> operators,
                  "NewlinesInGroups" -> groups,
                  "NewlinesBetweenCommas" -> commas,
                  "NewlinesInControl" -> ctrlStruct,
                  "NewlinesInScoping" -> scopingStruct,
                  "NewlinesInComments" -> comments|>;
              ];
            ]
        ],
        Background -> BackgroundCol, RoundingRadius -> 0, FrameMargins -> {{8, 8}, {8, 8}}
      ]
    ,
    (*
    WindowTitle->Dynamic[FEPrivate`FrontEndResource["CodeFormatterStrings", "PaletteTitle"]]

    Cannot use FrontEndResource for WindowTitle because $Failed will appear in FE Palettes Menu

    Related bugs: 401490
    *)
    WindowTitle -> FEPrivate`FrontEndResource["CodeFormatterStrings", "PaletteTitle"],
    Background -> BackgroundCol,
    MenuSortingValue -> 1150, (* Group the Code palettes together -- 416653 *)
    Saveable -> False,
    StyleDefinitions ->
      Notebook[
        {
          Cell[StyleData[StyleDefinitions -> "Palette.nb"]],
          Cell[StyleData["CodeFormatterHighlightColor"],
            System`EdgeColor -> GrayLevel[0.2],
            FontColor -> GrayLevel[0.2], (* for FrameStyle *)
            System`LineColor -> GrayLevel[0.2], (* for joined Line elements *)
            Opacity[1.]],
          Cell[StyleData["CodeFormatterNewlineColor"],
            FontColor -> RGBColor[1, 0.5, 0]],
          Cell[StyleData["CodeFormatterNewlineFillColor"],
            System`FrontFaceColor -> RGBColor[1, 0.5, 0],
            Opacity[1.]],
          Cell[StyleData["CodeFormatterNewlineFillColorSubtle"],
            System`FrontFaceColor -> RGBColor[0.988235, 0.882353, 0.780392],
            Opacity[1.]],
          Cell[StyleData["CodeFormatterTextBase"],
            FontFamily -> "Source Sans Pro",
            FontSize -> 13,
            FontWeight -> Plain,
            FontSlant -> Plain,
            LinebreakAdjustments -> {1, 10, 1, 0, 1},
            LineIndent -> 0,
            PrivateFontOptions -> {"OperatorSubstitution" -> False}],
          Cell[StyleData["CodeFormatterText", StyleDefinitions -> StyleData["CodeFormatterTextBase"]],
            FontColor -> GrayLevel[0.2]],
          Cell[StyleData["ButtonCommonOptions"],
            FrameBoxOptions -> {
              Alignment -> Center,
              FrameMargins -> 4,
              FrameStyle -> None,
              ImageSize -> {{38, Full}, {19.5, Full}},
              RoundingRadius -> 3}],
          Cell[StyleData["ButtonGray2Normal", StyleDefinitions -> StyleData["ButtonCommonOptions"]],
            FontColor->GrayLevel[0.2],
            Background->GrayLevel[1],
            FrameBoxOptions->{FrameStyle->{GrayLevel[166/255]}}],
          Cell[StyleData["ButtonGray2Hover", StyleDefinitions -> StyleData["ButtonCommonOptions"]],
            FontColor->GrayLevel[0.2],
            Background->GrayLevel[1],
            FrameBoxOptions->{FrameStyle->{GrayLevel[0.2]}}],
          Cell[StyleData["ButtonGray2Pressed", StyleDefinitions -> StyleData["ButtonCommonOptions"]],
            FontColor->GrayLevel[1],
            Background->GrayLevel[166/255]],
          Cell[StyleData["ButtonGray2Disabled", StyleDefinitions -> StyleData["ButtonCommonOptions"]],
            FontColor->GrayLevel[0.7, 0.5],
            Background->GrayLevel[1, 0.5],
            FrameBoxOptions->{FrameStyle->{GrayLevel[166/255, 0.5]}}]},
        StyleDefinitions -> "PrivateStylesheetFormatting.nb"]
  ];

  If[!MatchQ[nb, _NotebookObject],
    Print["CreatePalette failed: ", nb];
    Quit[1]
  ];

  Quiet[DeleteFile[FileNameJoin[{buildDir, "paclet", "CodeFormatter", "FrontEnd", "Palettes", "CodeFormatter.nb"}]], DeleteFile::fdnfnd];

  Print["saving CodeFormatter.nb"];
  res = NotebookSave[nb, FileNameJoin[{buildDir, "paclet", "CodeFormatter", "FrontEnd", "Palettes", "CodeFormatter.nb"}]];

  Print[res];

  If[res =!= Null,
    Quit[1]
  ];
  ];

  (*
  NotebookSave may fail, but give no indication,
  so need to explicitly check that file was created
  bug 429251
  *)
  If[!FileExistsQ[FileNameJoin[{buildDir, "paclet", "CodeFormatter", "FrontEnd", "Palettes", "CodeFormatter.nb"}]],
    Quit[1]
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
