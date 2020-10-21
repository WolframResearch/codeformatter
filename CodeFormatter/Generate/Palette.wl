BeginPackage["CodeFormatter`Generate`Palette`"]

Begin["`Private`"]

Needs["CodeFormatter`Generate`GenerateSources`"]

Print["Generating Palette..."]

$minAirinessIcon =
Graphics[{GrayLevel[0.5], AbsoluteThickness[1.5], CapForm["Butt"], 
  Line[{{Offset[{0, 4.949999999999999}, {0, 0}], Offset[{8.6, 4.949999999999999}, {0, 
      0}]}, {Offset[{10.6, 4.949999999999999}, {0, 0}], 
     Offset[{17.2, 4.949999999999999}, {0, 0}]}, {Offset[{0, 1.65}, {0, 0}], 
     Offset[{4.3, 1.65}, {0, 0}]}, {Offset[{6.3, 1.65}, {0, 0}], 
     Offset[{12.899999999999999, 1.65}, {0, 0}]}, 
    {Offset[{14.899999999999999, 1.65}, {0, 0}], Offset[{19.349999999999998, 1.65}, {0, 
      0}]}, {Offset[{0, -1.65}, {0, 0}], Offset[{6.449999999999999, -1.65}, {0, 0}]}, 
    {Offset[{8.45, -1.65}, {0, 0}], Offset[{16.34, -1.65}, {0, 0}]}, 
    {Offset[{0, -4.949999999999999}, {0, 0}], Offset[{10.75, -4.949999999999999}, {0, 
      0}]}, {Offset[{12.75, -4.949999999999999}, {0, 0}], 
     Offset[{17.2, -4.949999999999999}, {0, 0}]}}]}, ImagePadding -> All, 
 ImageSize -> {22, 15}, ImageMargins -> 10]


$maxAirinessIcon =
Graphics[{GrayLevel[0.5], AbsoluteThickness[1.5], CapForm["Butt"], 
  Line[{{Offset[{0, 4.949999999999999}, {0, 0}], Offset[{7.74, 4.949999999999999}, {0, 
      0}]}, {Offset[{4.3, 1.65}, {0, 0}], Offset[{10.75, 1.65}, {0, 0}]}, 
    {Offset[{8.6, -1.65}, {0, 0}], Offset[{15.049999999999999, -1.65}, {0, 0}]}, 
    {Offset[{4.3, -4.949999999999999}, {0, 0}], Offset[{8.6, -4.949999999999999}, {0, 
      0}]}}]}, ImagePadding -> All, ImageSize -> {17, 15}, ImageMargins -> 10]



generatePalette[] :=
Module[{nb},
  
  UsingFrontEnd[

    nb = CreatePalette[
      DynamicModule[{},
      Panel[Column[
      {
        Button[Dynamic[FEPrivate`FrontEndResource["CodeFormatterStrings", "PaletteFormatButton"]], Needs["CodeFormatter`Notebooks`"];CodeFormatter`Notebooks`formatSelectedCell[], Method -> "Queued"],
        Spacer[{0, 20}],
        (*
        I would like to use Labeled[content, label, {{Top, Left}}]

        But there is a bug that causes content to stay center-aligned when the label is longer than the content

        email thread sep 14 2020 [l-frontend] Aligning label and content in Labeled

        so just using Column[] for now

        In the future, make sure to do {{Top, Left}}, not {Top, Left}
        https://mathematica.stackexchange.com/a/126834/63281
        *)
        Column[{
          Style[Dynamic[FEPrivate`FrontEndResource["CodeFormatterStrings", "AirinessHeader"]], "Item"],
          Row[{
            $minAirinessIcon,
            Slider[Dynamic[CodeFormatter`$InteractiveAiriness, (
              CodeFormatter`$InteractiveAiriness = #;
              If[$VersionNumber >= 12.2,
                CurrentValue[$FrontEnd, {CodeAssistOptions, "CodeToolsOptions", "CodeFormatterInteractiveAiriness"}] = #;
              ];
              CodeFormatter`Notebooks`formatSelectedCell[];
              )&], {-1, 1}, ContinuousAction -> False
            ],
            $maxAirinessIcon
          }]
        }],
        Spacer[{0, 20}],
        OpenerView[{
          Dynamic[FEPrivate`FrontEndResource["CodeFormatterStrings", "SettingsHeader"]],
          Panel[Column[{
            Column[{
              Style[Dynamic[FEPrivate`FrontEndResource["CodeFormatterStrings", "IndentationCharacterHeader"]], "Item"],
              RadioButtonBar[Dynamic[CodeFormatter`$InteractiveIndentationCharacter, (
                CodeFormatter`$InteractiveIndentationCharacter = #;
                If[$VersionNumber >= 12.2,
                  CurrentValue[$FrontEnd, {CodeAssistOptions, "CodeToolsOptions", "CodeFormatterInteractiveIndentationCharacter"}] = #;
                ];
                (* CodeFormatter`Notebooks`formatSelectedCell[]; *)
                )&], {"tab" -> Dynamic[FEPrivate`FrontEndResource["CodeFormatterStrings", "TabMenuItem"]], "space" -> Dynamic[FEPrivate`FrontEndResource["CodeFormatterStrings", "SpaceMenuItem"]]}
              ]
            }],
            Spacer[{0, 20}],
            Column[{
              Style[Dynamic[FEPrivate`FrontEndResource["CodeFormatterStrings", "TabWidthHeader"]], "Item"],
              RadioButtonBar[Dynamic[CodeFormatter`$InteractiveTabWidth, (
                CodeFormatter`$InteractiveTabWidth = #;
                If[$VersionNumber >= 12.2,
                  CurrentValue[$FrontEnd, {CodeAssistOptions, "CodeToolsOptions", "CodeFormatterInteractiveTabWidth"}] = #;
                ];
                (* CodeFormatter`Notebooks`formatSelectedCell[]; *)
                )&], {"2", "4", "6", "8"}
              ]
            }],
            Spacer[{0, 20}],
            Column[{
              (*
              Reparsing boxes before formatting prevents several problems:
              
              Sometimes cells are hand-edited and are incorrect.

              The FE has bugs in the Package Editor that this fixes

              Related bugs: 395301
              *)
              Style[Dynamic[FEPrivate`FrontEndResource["CodeFormatterStrings", "ReparseHeader"]], "Item"],
              Checkbox[Dynamic[CodeFormatter`$InteractiveReparse, (
                CodeFormatter`$InteractiveReparse = #;
                If[$VersionNumber >= 12.2,
                  CurrentValue[$FrontEnd, {CodeAssistOptions, "CodeToolsOptions", "CodeFormatterInteractiveReparse"}] = #;
                ]
                (* CodeFormatter`Notebooks`formatSelectedCell[]; *)
                )&]
              ]
            }]
            (*
            ,
            Spacer[{0, 20}],
            Column[{
              Style["Enable Experimental Toolbar Format Button (12.2+)", "Item"],
              Checkbox[Dynamic[CurrentValue[$FrontEnd, {PrivateFrontEndOptions, "ExperimentalSettings", "CodeAssist", "ToolbarFormatButton"}, True]]]
            }]
            *)
          }]]
        }]
      }]], Initialization :> (
        Module[{opts},

          Needs["CodeFormatter`Notebooks`"];

          If[$VersionNumber >= 12.2,

            opts = CurrentValue[$FrontEnd, {CodeAssistOptions, "CodeToolsOptions"}];

            If[FailureQ[opts],
              opts = <||>
            ];

            CodeFormatter`$InteractiveAiriness = Lookup[opts, "CodeFormatterInteractiveAiriness", 0];
            CodeFormatter`$InteractiveTabWidth = Lookup[opts, "CodeFormatterInteractiveTabWidth", "4"];
            CodeFormatter`$InteractiveIndentationCharacter = Lookup[opts, "CodeFormatterInteractiveIndentationCharacter", "space"];
            CodeFormatter`$InteractiveReparse = Lookup[opts, "CodeFormatterInteractiveReparse", True];
          ];
        ])]
      ,
      (*
      WindowTitle->Dynamic[FEPrivate`FrontEndResource["CodeFormatterStrings", "PaletteTitle"]]

      Cannot use FrontEndResource for WindowTitle because $Failed will appear in FE Palettes Menu

      Related bugs: xxx
      *)
      WindowTitle->"Code Formatting"
    ];

    NotebookSave[nb, FileNameJoin[{generatedWLDir, "CodeFormatter.nb"}]]
  ]
]

Print["UsingFrontEnd... \[WatchIcon]"]

generatePalette[]

Print["Done Palette"]

End[]

EndPackage[]
