BeginPackage["CodeFormatter`Notebooks`"]


formatCurrentCell

formatCurrentNotebook



Begin["`Private`"]

Needs["CodeFormatter`"]

Needs["CodeParser`"]





formatCurrentCell[] :=
  Catch[
  Module[{nb, read, formatted, toWrite},

    nb = InputNotebook[];

    CurrentValue[nb, WindowStatusArea] = "formatting cell...";

    SelectionMove[nb, All, Cell];

    read = NotebookRead[nb];

    If[$reparse,
        read = FrontEndExecute[FrontEnd`ReparseBoxStructurePacket[read]]
    ];

    Switch[read,
      Cell[_, "Program", ___],
        formatted = formatProgramCellContents[read[[1]]];
        toWrite = read;
        toWrite[[1]] = formatted;
      ,
      Cell[BoxData[_], "Input" | "Code", ___],
        formatted = formatInputContents[read[[1, 1]]];
        toWrite = read;
        toWrite[[1, 1]] = formatted;
      ,
      _,
        toWrite = read
    ];

    NotebookWrite[nb, toWrite];

    CurrentValue[nb, WindowStatusArea] = "";

  ]]


formatCurrentNotebook[] :=
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




formatProgramCellContents[contents_String] :=
    Catch[
    Module[{formatted},
        formatted = CodeFormat[contents];
        If[FailureQ[formatted],
            Throw[formatted]
        ];
        formatted = StringTrim[formatted, "\n"..];
        formatted
    ]]

formatInputContents[contentsBox_] :=
    Catch[
    Module[{cst, formatted, formattedBox},
        (*
        convert boxes to form that is understood by formatter
        *)
        cst = CodeConcreteParseBox[contentsBox];
        If[FailureQ[cst],
            Throw[cst]
        ];
        formatted = CodeFormatCST[cst];
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



End[]

EndPackage[]
