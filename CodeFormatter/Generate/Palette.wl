BeginPackage["CodeFormatter`Generate`Palette`"]

Begin["`Private`"]

Needs["CodeFormatter`Generate`GenerateSources`"]

Print["Generating Palette..."]

generatePalette[] :=
Module[{nb},
  
  UsingFrontEnd[

    nb = CreatePalette[
      DynamicModule[{},
      Panel[Column[
      {
        Button["Format selected cell", Needs["CodeFormatter`Notebooks`"];CodeFormatter`Notebooks`formatSelectedCell[], Method -> "Queued"],
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
          Style["Airiness", "Item"],
          Slider[Dynamic[CodeFormatter`$InteractiveAiriness, (
            CodeFormatter`$InteractiveAiriness = #;
            If[$VersionNumber >= 12.2,
              CurrentValue[$FrontEnd, {CodeAssistOptions, "CodeToolsOptions", "CodeFormatterInteractiveAiriness"}] = #;
            ];
            CodeFormatter`Notebooks`formatSelectedCell[];
            )&], {-1, 1}, ContinuousAction -> False
          ]
        }],
        Spacer[{0, 20}],
        OpenerView[{
          "Settings",
          Panel[Column[{
            Column[{
              Style["Indentation character", "Item"],
              RadioButtonBar[Dynamic[CodeFormatter`$InteractiveIndentationCharacter, (
                CodeFormatter`$InteractiveIndentationCharacter = #;
                If[$VersionNumber >= 12.2,
                  CurrentValue[$FrontEnd, {CodeAssistOptions, "CodeToolsOptions", "CodeFormatterInteractiveIndentationCharacter"}] = #;
                ];
                (* CodeFormatter`Notebooks`formatSelectedCell[]; *)
                )&], {"tab", "space"}
              ]
            }],
            Spacer[{0, 20}],
            Column[{
              Style["Tab width", "Item"],
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
              Style["Reparse boxes before formatting", "Item"],
              Checkbox[Dynamic[CodeFormatter`$InteractiveReparse, (
                CodeFormatter`$InteractiveReparse = #;
                If[$VersionNumber >= 12.2,
                  CurrentValue[$FrontEnd, {CodeAssistOptions, "CodeToolsOptions", "CodeFormatterInteractiveReparse"}] = #;
                ]
                (* CodeFormatter`Notebooks`formatSelectedCell[]; *)
                )&]
              ]
            }]
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
        ])], WindowTitle->"CodeFormatter"];

    NotebookSave[nb, FileNameJoin[{generatedWLDir, "CodeFormatter.nb"}]]
  ]
]

Print["UsingFrontEnd... \[WatchIcon]"]

generatePalette[]

Print["Done Palette"]

End[]

EndPackage[]
