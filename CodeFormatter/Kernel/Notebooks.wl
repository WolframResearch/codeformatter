BeginPackage["CodeFormatter`Notebooks`"]


formatSelectedCell

(*
formatSelectedNotebook
*)


Begin["`Private`"]

Needs["CodeFormatter`"]

Needs["CodeParser`"]





formatSelectedCell[] :=
  Catch[
  Module[{nb, read, formatted, toWrite, shouldWrite},

    nb = InputNotebook[];

    (*
    first try determining the kind of selection
    *)
    read = NotebookRead[nb];

    Switch[read,
        _Cell | {_Cell...},
            (*
            plain cursor, single cell selected, multiple cells selected
            *)
            Null
        ,
        _,
            (*
            Anything else
            Some selection of boxes
            Not supported right now
            Return immediately
            *)
            Throw[Null]
    ];

    SelectionMove[nb, All, Cell, AutoScroll -> False];

    read = NotebookRead[nb];

    Switch[read,
        _RowBox,
            (*
            Some selection of boxes
            Not supported right now
            Return immediately
            *)
            Throw[Null]
        ,
        Cell[___],
            (*
            wrap List around single Cell, then proceed
            *)
            read = {read}
    ];

    shouldWrite = False;

    CurrentValue[nb, WindowStatusArea] = "formatting selected cell...";

    toWrite = MapIndexed[Function[{cell, index},
        CurrentValue[nb, WindowStatusArea] = "formatting selected cell... " <> ToString[Floor[100 index[[1]]/Length[read]]] <> "%";
        Switch[cell,
          Cell[_, "Program", ___],

            shouldWrite = True;

            formatted = cell;

            If[CodeFormatter`$InteractiveReparse,
                formatted = FrontEndExecute[FrontEnd`ReparseBoxStructurePacket[formatted]]
            ];

            formatted = formatProgramCellContents[formatted[[1]]];
            Cell[formatted, Sequence @@ cell[[2;;]]]
          ,
          (*
          only RowBoxes are supported for now
          *)
          Cell[BoxData[b_], "Input" | "Code", ___] /; Union[Cases[b, _Symbol, Infinity, Heads -> True]] === {List, RowBox},

            shouldWrite = True;

            formatted = cell;

            If[CodeFormatter`$InteractiveReparse,
                formatted = FrontEndExecute[FrontEnd`ReparseBoxStructurePacket[formatted]]
            ];

            formatted = formatInputContents[formatted[[1, 1]]];
            Cell[BoxData[formatted], Sequence @@ cell[[2;;]]]
          ,
          _,
            cell
        ]
    ], read];

    If[shouldWrite,
        NotebookWrite[nb, toWrite, All];
    ];

    CurrentValue[nb, WindowStatusArea] = "";

  ]]

(*

formatSelectedNotebook[] is currently disabled because of a bug in the FE that prevents undo from working after NotebookPut

Related bugs: 395592
*)
(*
formatSelectedNotebook[] :=
  Catch[
  Module[{nb, read, toWrite, cells, cellsToWrite},

    nb = InputNotebook[];

    CurrentValue[nb, WindowStatusArea] = "formatting notebook... 0%";

    read = NotebookGet[nb];

    $LastNBRead = read;

    cells = read[[1]];

    If[$reparse,
        cells = FrontEndExecute[FrontEnd`ReparseBoxStructurePacket[#]]& /@ cells
    ];

    cellsToWrite =
        MapIndexed[Function[{cell, index},
            CurrentValue[nb, WindowStatusArea] = "formatting notebook... " <> ToString[Floor[100 index[[1]]/Length[cells]]] <> "%";
            Switch[cell,
              Cell[_, "Program", ___],
                ReplacePart[cell, {1} -> formatProgramCellContents[cell[[1]]]]
              ,
              Cell[BoxData[_], "Input" | "Code", ___],
                ReplacePart[cell, {1, 1} -> formatInputContents[cell[[1, 1]]]]
              ,
              _,
                cell
            ]
        ], cells];

    toWrite = read;
    toWrite[[1]] = cellsToWrite;

    $LastNBToWrite = toWrite;

    NotebookPut[toWrite, nb];

    CurrentValue[nb, WindowStatusArea] = "";
  ]]
*)



formatProgramCellContents[contents_String] :=
    Catch[
    Module[{formatted, airiness, indentationString, tabWidth},

        airiness = CodeFormatter`$InteractiveAiriness;
        tabWidth = massageTabWidth[CodeFormatter`$InteractiveTabWidth];
        indentationString = massageIndentationString[CodeFormatter`$InteractiveIndentationCharacter, tabWidth];

        (*
        FIXME: "LineWidth" -> Infinity will be revisited when line continuations in boxes are properly supported
        *)
        formatted = CodeFormat[contents, Airiness -> airiness, "LineWidth" -> Infinity, "IndentationString" -> indentationString, "TabWidth" -> tabWidth];
        If[FailureQ[formatted],
            Throw[formatted]
        ];
        formatted = StringTrim[formatted, "\n"..];
        formatted
    ]]

formatInputContents[contentsBox_] :=
    Catch[
    Module[{cst, formatted, formattedBox, airiness, indentationString, tabWidth},

        airiness = CodeFormatter`$InteractiveAiriness;
        tabWidth = massageTabWidth[CodeFormatter`$InteractiveTabWidth];
        indentationString = massageIndentationString[CodeFormatter`$InteractiveIndentationCharacter, tabWidth];

        (*
        convert boxes to form that is understood by formatter
        *)
        cst = CodeConcreteParseBox[contentsBox];
        If[FailureQ[cst],
            Throw[cst]
        ];

        (*
        FIXME: "LineWidth" -> Infinity will be revisited when line continuations in boxes are properly supported
        *)
        formatted = CodeFormatCST[cst, Airiness -> airiness, "LineWidth" -> Infinity, "IndentationString" -> indentationString, "TabWidth" -> tabWidth];
        If[FailureQ[formatted],
            Throw[formatted]
        ];
        formatted = StringTrim[formatted, "\n"..];

        cst = CodeConcreteParse[formatted];
        If[FailureQ[cst],
            Throw[cst]
        ];
        (*
        trick ToStandardFormBoxes into thinking that cst came from boxes
        *)
        cst[[1]] = Box;
        formattedBox = ToStandardFormBoxes[cst];
        If[FailureQ[formattedBox],
            Throw[formattedBox]
        ];
        formattedBox
    ]]


massageIndentationString[s_String, tabWidth_] :=
    Module[{},
        Switch[s,
            "space",
                StringRepeat[" ", tabWidth]
            ,
            "tab",
                "\t"
            ,
            _,
                StringRepeat[" ", tabWidth]
        ]
    ]

massageIndentationString[___] := $DefaultIndentationString


massageTabWidth[s_String] :=
    Module[{parsed},
        parsed = CodeConcreteParseLeaf[s];
        Switch[parsed,
            LeafNode[Integer, _, _],
                FromNode[parsed]
            ,
            _,
                $DefaultTabWidth
        ]
    ]

massageTabWidth[_] := $DefaultTabWidth







End[]

EndPackage[]
