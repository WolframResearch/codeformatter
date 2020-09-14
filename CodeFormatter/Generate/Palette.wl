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
        Labeled[Slider[Dynamic[CodeFormatter`$InteractiveAiriness, (
          CodeFormatter`$InteractiveAiriness = #;
          If[$VersionNumber >= 12.2,
            CurrentValue[$FrontEnd, {CodeAssistOptions, "CodeToolsOptions", "CodeFormatterInteractiveAiriness"}] = #;
          ];
          CodeFormatter`Notebooks`formatSelectedCell[];
          )&], {-1, 1}, ContinuousAction -> False], "Airiness"],
        Spacer[{0, 20}],
        OpenerView[{
          "Settings",
          Panel[Column[{
            Labeled[RadioButtonBar[Dynamic[CodeFormatter`$InteractiveIndentationCharacter, (
              CodeFormatter`$InteractiveIndentationCharacter = #;
              If[$VersionNumber >= 12.2,
                CurrentValue[$FrontEnd, {CodeAssistOptions, "CodeToolsOptions", "CodeFormatterInteractiveIndentationCharacter"}] = #;
              ];
              (* CodeFormatter`Notebooks`formatSelectedCell[]; *)
              )&], {"tab", "space"}], "Indentation character"],
            Spacer[{0, 20}],
            Labeled[RadioButtonBar[Dynamic[CodeFormatter`$InteractiveTabWidth, (
              CodeFormatter`$InteractiveTabWidth = #;
              If[$VersionNumber >= 12.2,
                CurrentValue[$FrontEnd, {CodeAssistOptions, "CodeToolsOptions", "CodeFormatterInteractiveTabWidth"}] = #;
              ];
              (* CodeFormatter`Notebooks`formatSelectedCell[]; *)
              )&], {"2", "4", "6", "8"}], "Tab width"],
            Spacer[{0, 20}],
            Labeled[Checkbox[Dynamic[CodeFormatter`$InteractiveReparse, (
              CodeFormatter`$InteractiveReparse = #;
              If[$VersionNumber >= 12.2,
                CurrentValue[$FrontEnd, {CodeAssistOptions, "CodeToolsOptions", "CodeFormatterInteractiveReparse"}] = #;
              ]
              (* CodeFormatter`Notebooks`formatSelectedCell[]; *)
              )&]], "Reparse boxes before formatting"]
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
