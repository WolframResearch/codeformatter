BeginPackage["Format`AggregateRules`"]

$DefaultAggregateRules


Begin["`Private`"]

Needs["Format`"]
Needs["AST`Utils`"]
Needs["AST`"]




(*

Rules are of the form: pat -> func where pat is the node pattern to match on and func is the processing function for the node.

Functions are of the form: function[pos_, ast_] where pos is the position of the node in the AST, and ast is the AST itself.
  And function must return a list of Lints. 


A rule of thumb is to make patterns as specific as possible, to offload work of calling the function.

*)

$DefaultAggregateRules = <|

InfixNode[Times, children_ /; !FreeQ[children, LeafNode[Token`Fake`ImplicitTimes, _, _], 1], _] -> scanImplicitTimes,

BinaryNode[Except[Power | PatternTest | Pattern | Optional], _, _] -> scanBinaryNode,

InfixNode[Except[CompoundExpression | Comma | MessageName], _, _] -> scanInfixNode,

InfixNode[Comma, _, _] -> scanComma,

InfixNode[CompoundExpression, _, _] -> scanCompoundExpression,

Nothing
|>



Attributes[scanImplicitTimes] = {HoldRest}

scanImplicitTimes[pos_List, aggIn_] :=
Catch[
Module[{agg, node, data, children, issues, pairs, src},
  agg = aggIn;
  node = Extract[agg, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  pairs = Partition[children, 2, 1];

  If[$Debug,
    Print["pairs: ", pairs];
  ];

  Do[

    If[!MatchQ[p[[2]], LeafNode[Token`Fake`ImplicitTimes, _, _]],
      Continue[]
    ];

    If[!contiguousQ[p[[1, 3, Key[Source] ]], p[[2, 3, Key[Source] ]] ],
      Continue[]
    ];

    src = p[[2, 3, Key[Source] ]];

    AppendTo[issues, FormatIssue["ContiguousImplicitTimes", "Contiguous implicit Times", "Formatting",
      <|Source -> src,
        AirynessLevel -> 0.2,
        CodeActions -> {
          CodeAction["Insert space", InsertNode, <|Source -> src, "InsertionNode" -> LeafNode[Whitespace, " ", <||>]|>] }
      |>]];
    ,
    {p, pairs}
  ];

  issues
]]


Attributes[scanBinaryNode] = {HoldRest}

scanBinaryNode[pos_List, aggIn_] :=
Catch[
Module[{agg, node, data, children, issues, pairs, src},
  agg = aggIn;
  node = Extract[agg, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  pairs = Partition[children, 2, 1];

  If[$Debug,
    Print["pairs: ", pairs];
  ];

  Do[
    If[!contiguousQ[p[[1, 3, Key[Source] ]], p[[2, 3, Key[Source] ]] ],
      Continue[]
    ];

    src = p[[2, 3, Key[Source] ]];

    AppendTo[issues, FormatIssue["ContiguousBinary", "Contiguous binary", "Formatting",
      <|Source -> src,
        AirynessLevel -> 0.6,
        CodeActions -> {
          CodeAction["Insert space", InsertNode, <|Source -> src, "InsertionNode" -> LeafNode[Whitespace, " ", <||>]|>] }
      |>]];
    ,
    {p, pairs}
  ];

  issues
]]



Attributes[scanInfixNode] = {HoldRest}

scanInfixNode[pos_List, aggIn_] :=
Catch[
Module[{agg, node, data, children, issues, pairs, src},
  agg = aggIn;
  node = Extract[agg, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  pairs = Partition[children, 2, 1];

  If[$Debug,
    Print["pairs: ", pairs];
  ];

  Do[
    If[!contiguousQ[p[[1, 3, Key[Source] ]], p[[2, 3, Key[Source] ]] ],
      Continue[]
    ];

    src = p[[2, 3, Key[Source] ]];

    AppendTo[issues, FormatIssue["ContiguousInfix", "Contiguous infix", "Formatting",
      <|Source -> src,
        AirynessLevel -> 0.6,
        CodeActions -> {
          CodeAction["Insert space", InsertNode, <|Source -> src, "InsertionNode" -> LeafNode[Whitespace, " ", <||>]|>] }
      |>]];
    ,
    {p, pairs}
  ];

  issues
]]



Attributes[scanComma] = {HoldRest}

scanComma[pos_List, aggIn_] :=
Catch[
Module[{agg, node, data, children, issues, pairs, src},
  agg = aggIn;
  node = Extract[agg, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  (*
  Only insert space after the ,
  *)
  pairs = Partition[children, 2, 1];

  If[$Debug,
    Print["pairs: ", pairs];
  ];

  Do[
    If[p[[1, 1]] =!= Token`Comma,
      If[$Debug,
    Print["continue1"];
  ];
      Continue[]
    ];
    If[!contiguousQ[p[[1, 3, Key[Source] ]], p[[2, 3, Key[Source] ]] ],
      If[$Debug,
    Print["continue2"];
  ];
      Continue[]
    ];

    src = p[[2, 3, Key[Source] ]];

    AppendTo[issues, FormatIssue["ContiguousComma", "Contiguous comma", "Formatting",
      <|Source -> src,
        AirynessLevel -> 0.6,
        CodeActions -> {
          CodeAction["Insert space", InsertNode, <|Source -> src, "InsertionNode" -> LeafNode[Whitespace, " ", <||>]|>] }
      |>]];
    ,
    {p, pairs}
  ];

  If[$Debug,
    Print["issues: ", issues];
  ];

  issues
]]





Attributes[scanCompoundExpression] = {HoldRest}

scanCompoundExpression[pos_List, aggIn_] :=
Catch[
Module[{agg, node, data, children, issues, pairs, src},
  agg = aggIn;
  node = Extract[agg, {pos}][[1]];
  children = node[[2]];
  data = node[[3]];

  issues = {};

  (*
  Only insert space after the ;
  *)
  pairs = Partition[children, 2, 1];

  If[$Debug,
    Print["pairs: ", pairs];
  ];

  Do[
    If[p[[1, 1]] =!= Token`Semi,
      Continue[]
    ];
    If[!contiguousQ[p[[1, 3, Key[Source] ]], p[[2, 3, Key[Source] ]] ],
      Continue[]
    ];

    src = p[[2, 3, Key[Source] ]];

    AppendTo[issues, FormatIssue["ContiguousSemi", "Contiguous semi", "Formatting",
      <|Source -> src,
        AirynessLevel -> 0.6,
        CodeActions -> {
          CodeAction["Insert space", InsertNode, <|Source -> src, "InsertionNode" -> LeafNode[Whitespace, " ", <||>]|>] }
      |>]];
    ,
    {p, pairs}
  ];

  issues
]]



End[]


EndPackage[]
