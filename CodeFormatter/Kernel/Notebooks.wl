BeginPackage["CodeFormatter`Notebooks`"]


formatCurrentCell


Begin["`Private`"]

Needs["CodeFormatter`"]

Needs["CodeParser`"]





formatCurrentCell[] :=
  Catch[
  Module[{nb, read, formatted, cst, box},

    nb = InputNotebook[];

    read = NotebookRead[nb];

    Switch[read,
      Cell[_, "Program", ___],
        formatted = CodeFormat[read[[1]]];
        If[FailureQ[formatted],
            Throw[formatted]
        ];
        formatted = StringTrim[formatted, "\n"..];
        read[[1]] = formatted;
        NotebookWrite[nb, read]
      ,
      Cell[_, "Input" | "Code", ___],
        (*
        convert boxes to form that is understood by formatter
        *)
        cst = CodeConcreteParseBox[read[[1, 1]]];
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
        box = ToStandardFormBoxes[cst];
        If[FailureQ[box],
            Throw[box]
        ];
        read[[1, 1]] = box;
        NotebookWrite[nb, read]
    ]

  ]]






End[]

EndPackage[]
