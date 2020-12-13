BeginPackage["CodeFormatter`Notebooks`"]


formatSelectedCell

(*
formatSelectedNotebook
*)


Begin["`Private`"]

Needs["CodeFormatter`"]
Needs["CodeFormatter`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


formatSelectedCell[] :=
  Catch[
  Module[{nb, read, formatted, toWrite, shouldWrite, failure},

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
    failure = False;

    CurrentValue[nb, WindowStatusArea] = "Formatting selected cell...";

    toWrite = MapIndexed[Function[{cell, index},
        Function[val, CurrentValue[nb, WindowStatusArea] = "Formatting selected cell... " <> ToString[Floor[100 index[[1]]/Length[read]]] <> "%"; val]@
        Switch[cell,
          Cell[_, "Program", ___],

            shouldWrite = True;

            formatted = cell;

            If[CodeFormatter`$InteractiveReparse,
                formatted = FrontEndExecute[FrontEnd`ReparseBoxStructurePacket[formatted]]
            ];

            formatted = formatProgramCellContents[formatted[[1]]];

            If[FailureQ[formatted],

                failure = True;

                CurrentValue[nb, WindowStatusArea] = "";
                
                Throw[formatted]
            ];

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

            If[FailureQ[formatted],

                failure = True;

                CurrentValue[nb, WindowStatusArea] = "";

                (*
                Ideally, this would be a Beep[] and populate the Why the Beep? dialog

                But that is not really possible

                So just do a message for now
                
                Related threads: https://mail-archive.wolfram.com/archive/l-kernel/2020/Aug00/0035.html
                *)
                Message[CodeFormat::cellfailure];

                Throw[formatted]
            ];

            Cell[BoxData[formatted], Sequence @@ cell[[2;;]]]
          ,
          (*
          Also support Code cells that have been evaluated and have Output grouped with them
          *)
          Cell[CellGroupData[{Cell[BoxData[b_], "Input" | "Code", ___] /; Union[Cases[b, _Symbol, Infinity, Heads -> True]] === {List, RowBox}, Cell[_, "Output", ___]}, _]],

            shouldWrite = True;

            (*
            The Input cell
            *)
            formatted = cell[[1, 1, 1]];

            If[CodeFormatter`$InteractiveReparse,
                formatted = FrontEndExecute[FrontEnd`ReparseBoxStructurePacket[formatted]]
            ];

            formatted = formatInputContents[formatted[[1, 1]]];

            If[FailureQ[formatted],

                failure = True;

                CurrentValue[nb, WindowStatusArea] = "";

                Message[CodeFormat::cellfailure];

                Throw[formatted]
            ];

            Cell[CellGroupData[{Cell[BoxData[formatted], Sequence @@ cell[[1, 1, 1, 2;;]]], cell[[1, 1, 2]]}, cell[[1, 2]]]]
          ,
          (*
          Some other box
          *)
          _,
            Message[CodeFormat::cellfailure];
            cell
        ]
    ], read];

    If[shouldWrite && !failure,
        NotebookWrite[nb, toWrite, All]
    ];

    CurrentValue[nb, WindowStatusArea] = "";

    Null
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


(*
Return string
*)
formatProgramCellContents[contents_String] :=
    Catch[
    Module[{formatted, airiness, indentationString, tabWidth},

        airiness = massageAiriness[CodeFormatter`$InteractiveAiriness];
        tabWidth = massageTabWidth[CodeFormatter`$InteractiveTabWidth];
        indentationString = massageIndentationString[CodeFormatter`$InteractiveIndentationCharacter, tabWidth];

        formatted = CodeFormat[contents, Airiness -> airiness, "IndentationString" -> indentationString, "TabWidth" -> tabWidth];
        If[FailureQ[formatted],
            Throw[formatted]
        ];
        formatted = StringTrim[formatted, "\n"..];
        formatted
    ]]

(*
Return boxes
*)
formatInputContents[contentsBox_] :=
    Catch[
    Module[{cst, formatted, formattedBox, airiness, indentationString, tabWidth, agg, cst2, agg2, aggToCompare, agg2ToCompare, newline,
        issues},

        airiness = massageAiriness[CodeFormatter`$InteractiveAiriness];
        tabWidth = massageTabWidth[CodeFormatter`$InteractiveTabWidth];
        indentationString = massageIndentationString[CodeFormatter`$InteractiveIndentationCharacter, tabWidth];
        newline = "\n";

        (*
        convert boxes to form that is understood by formatter
        *)
        cst = CodeConcreteParseBox[contentsBox];
        If[FailureQ[cst],
            Throw[cst]
        ];

        formatted = CodeFormatCST[cst, Airiness -> airiness, "IndentationString" -> indentationString, "TabWidth" -> tabWidth];
        If[FailureQ[formatted],
            Throw[formatted]
        ];

        formatted = StringTrim[formatted, "\n"..];


        (*
        CodeFormatCST does not do sanity checking, so must do it here
        *)
        If[CodeFormatter`Private`$DisableSanityChecking,

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
        ];

        (*
        Sanity Checking
        *)

        agg = CodeParser`Abstract`Aggregate[cst];
        agg = expandMultiSingleQuote[agg];
        agg = expandTernaryOptionalPattern[agg];
        agg = expandEqualDot[agg];
        agg = coalesceLineContinuation[agg];
        agg = flattenGroupMissingCloserNode[agg];

        cst2 = CodeConcreteParse[formatted];

        If[MatchQ[cst2[[3]], KeyValuePattern[SyntaxIssues -> {___, SyntaxIssue["UnrecognizedCharacter", _, _, _], ___}]],
            
            issues = Lookup[cst2[[3]], SyntaxIssues];
            issues = boldify[#[[2]]]& /@ issues;

            (*
            This message is indicating that there is a syntax error in the input, such as:
            f["\."]
            *)
            Message[CodeFormat::syntaxissues, issues]
        ];

        agg2 = CodeParser`Abstract`Aggregate[cst2];
        agg2 = reduceSpan[agg2];
        agg2 = preferGroupMissingCloserNode[agg2];

        agg2[[1]] = Box;

        aggToCompare = agg /. _Association -> <||>;
        agg2ToCompare = agg2 /. _Association -> <||>;

        If[aggToCompare =!= agg2ToCompare,
            If[$Debug,
              Print["aggToCompare: ", aggToCompare];
              Print["agg2ToCompare: ", agg2ToCompare]
            ];
            Throw[Failure["SanityCheckFailed", <||>]]
        ];


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


(*
boxes use a single token for '''
and text uses multiple tokens

must make sure that they are the same when we do a sanity check

it is easier to expand the single token into multiple tokens
*)
expandMultiSingleQuote[agg_] :=
    agg /. {
        PostfixNode[Derivative, {rand_, LeafNode[Token`Boxes`MultiSingleQuote, s_, _]}, _] :>
            Nest[PostfixNode[Derivative, {#, LeafNode[Token`SingleQuote, "'", <||>]}, <||>]&, rand, StringLength[s]]
    }

(*
boxes use a single RowBox for a:b:c
and text uses 2 nested BinaryNodes

must make sure that they are the same when we do a sanity check

it is easier to expand the single RowBox into multiple BinaryNodes
*)
expandTernaryOptionalPattern[agg_] :=
    agg //. {
        TernaryNode[TernaryOptionalPattern, {a_, op1_, b_, op2_, c_}, _] :>
            BinaryNode[Optional, {BinaryNode[Pattern, {a, op1, b}, <||>], op2, c}, <||>]
    }

expandEqualDot[agg_] :=
    agg /. {
        BinaryNode[Unset, {rand_, LeafNode[Token`Boxes`EqualDot, _, _]}, _] :>
            BinaryNode[Unset, {rand, LeafNode[Token`Equal, "=", <||>], LeafNode[Token`Dot, ".", <||>]}, <||>]
    }

(*
A single ;; does not create a RowBox, so is not parsed correctly
Need to reduce the correctly parsed ;; with implicit tokens to a single ;;
*)
reduceSpan[agg_] :=
    agg /. {
        BinaryNode[Span, {LeafNode[Token`Fake`ImplicitOne, _, _], tok:LeafNode[Token`SemiSemi, _, _], LeafNode[Token`Fake`ImplicitAll, _, _]}, _] :>
            tok
    }

coalesceLineContinuation[aggIn_] :=
    Module[{agg, poss},

        agg = aggIn;

        poss = Position[agg, {___, BoxNode[RowBox, {{_, LeafNode[Token`Boxes`LineContinuation, _, _]}}, _], ___}];

        agg = MapAt[lcReplace1, agg, poss];

        poss = Position[agg, {___, CodeParser`LeafNode[Token`Boxes`LineContinuation, _, _], ___}];

        agg = MapAt[lcReplace2, agg, poss];

        agg
    ]

lcReplace1[l_] :=
    SequenceReplace[l, {BoxNode[RowBox, {{first_, LeafNode[Token`Boxes`LineContinuation, lc_, _]}}, _], LeafNode[tag_, str_, _]} :>
        Sequence @@ {first, LeafNode[tag, lc <> str, <||>]}]

lcReplace2[{LeafNode[Token`Boxes`LineContinuation, lc_, _], GroupNode[tag_, {LeafNode[tag1_, str_, _], rest___}, data_]}] :=
    GroupNode[tag, {LeafNode[tag1, lc <> str, <||>], rest}, data]

lcReplace2[l_] :=
    SequenceReplace[l, {
            {LeafNode[Token`Boxes`LineContinuation, lc_, _], LeafNode[tag_, str_, data_]} :>
                LeafNode[tag, lc <> str, data]
            ,
            {LeafNode[Token`Boxes`LineContinuation, lc_, _], PrefixNode[tag_, {LeafNode[tag1_, str_, _], rest___}, data_]} :>
                PrefixNode[tag, {LeafNode[tag1, lc <> str, <||>], rest}, data]
        }
    ]

(*
Parsing:

f[

as a string gives UnterminatedGroupNode

But parsing RowBox[{"f", "["}] gives GroupMissingCloserNode

Convert all UnterminatedGroupNode to GroupMissingCloserNode

*)
preferGroupMissingCloserNode[agg_] :=
    agg /. UnterminatedGroupNode -> GroupMissingCloserNode

(*
Parsing:

f[

as a string leaves all of the subsequent content unparsed

so try to reproduce that behavior with boxes

*)
flattenGroupMissingCloserNode[agg_] :=
    agg /. GroupMissingCloserNode[tag_, children_, data_] :> GroupMissingCloserNode[tag, Flatten[flatten /@ children], data]

flatten[n:LeafNode[_, _, _]] := n

flatten[_[_, children_List, _]] := flatten /@ children





(*
replace `` and ** markup
*)
boldify[s_String] :=
    StringReplace[s, {
        RegularExpression["``(.*?)``"] :> ToString[Style["$1", Bold], StandardForm],
        RegularExpression["\\*\\*(.*?)\\*\\*"] :> ToString[Style["$1", Bold], StandardForm],
        RegularExpression["\\?\\?(.*?)\\?\\?"] :> "$1"}
    ]




massageAiriness[a_Real] := a
massageAiriness[a_Integer] := a

massageAiriness[_] := $DefaultAiriness


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
