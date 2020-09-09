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
          CurrentValue[$FrontEnd, {CodeAssistOptions, "LinterOptions", "CodeFormatterInteractiveAiriness"}] = #;
          CodeFormatter`Notebooks`formatSelectedCell[];
          )&], {-1, 1}, ContinuousAction -> False], "Airiness"],
        Spacer[{0, 20}],
        OpenerView[{
          "Settings",
          Panel[Column[{
            Labeled[RadioButtonBar[Dynamic[CodeFormatter`$InteractiveIndentationCharacter, (
              CodeFormatter`$InteractiveIndentationCharacter = #;
              CurrentValue[$FrontEnd, {CodeAssistOptions, "LinterOptions", "CodeFormatterInteractiveIndentationCharacter"}] = #;
              (* CodeFormatter`Notebooks`formatSelectedCell[]; *)
              )&], {"tab", "space"}], "Indentation character"],
            Spacer[{0, 20}],
            Labeled[RadioButtonBar[Dynamic[CodeFormatter`$InteractiveTabWidth, (
              CodeFormatter`$InteractiveTabWidth = #;
              CurrentValue[$FrontEnd, {CodeAssistOptions, "LinterOptions", "CodeFormatterInteractiveTabWidth"}] = #;
              (* CodeFormatter`Notebooks`formatSelectedCell[]; *)
              )&], {"2", "4", "6", "8"}], "Tab width"],
            Spacer[{0, 20}],
            Labeled[Checkbox[Dynamic[CodeFormatter`$InteractiveReparse, (
              CodeFormatter`$InteractiveReparse = #;
              CurrentValue[$FrontEnd, {CodeAssistOptions, "LinterOptions", "CodeFormatterInteractiveReparse"}] = #;
              (* CodeFormatter`Notebooks`formatSelectedCell[]; *)
              )&]], "Reparse boxes before formatting"]
          }]]
        }]
      }]], Initialization :> (
        Module[{opts},

          Needs["CodeFormatter`Notebooks`"];
          
          opts = CurrentValue[$FrontEnd, {CodeAssistOptions, "LinterOptions"}];

          CodeFormatter`$InteractiveAiriness = Lookup[opts, "CodeFormatterInteractiveAiriness", 0];
          CodeFormatter`$InteractiveTabWidth = Lookup[opts, "CodeFormatterInteractiveTabWidth", "4"];
          CodeFormatter`$InteractiveIndentationCharacter = Lookup[opts, "CodeFormatterInteractiveIndentationCharacter", "space"];
          CodeFormatter`$InteractiveReparse = Lookup[opts, "CodeFormatterInteractiveReparse", True];
        ])], WindowTitle->"CodeFormatter"];

    NotebookSave[nb, FileNameJoin[{generatedWLDir, "CodeFormatter.nb"}]]
  ]
]

Print["UsingFrontEnd... \[WatchIcon]"]

generatePalette[]

Print["Done Palette"]

End[]

EndPackage[]
