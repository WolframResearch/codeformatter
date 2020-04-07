BeginPackage["CodeFormatter`ConcreteRules`"]

$DefaultConcreteRules


Begin["`Private`"]

Needs["CodeFormatter`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]



(*

Rules are of the form: pat -> func where pat is the node pattern to match on and func is the processing function for the node.

Functions are of the form: function[pos_, ast_] where pos is the position of the node in the AST, and ast is the AST itself.
  And function must return a list of Lints. 


A rule of thumb is to make patterns as specific as possible, to offload work of calling the function.

*)

$DefaultConcreteRules = <|

(*
BinaryNode[Span, _, _] -> scanBinarySpans,
*)

(*
TernaryNode[Span, _, _] -> scanTernarySpans,
*)

CallNode[{_, ___, LeafNode[Token`Newline, _, _], ___}, _, _] -> scanCalls,

PrefixNode[_, {___, LeafNode[Token`Newline, _, _], ___}, _] -> scanPrefixs,

PostfixNode[_, {___, LeafNode[Token`Newline, _, _], ___}, _] -> scanPostfixs,


Nothing
|>













Attributes[scanCalls] = {HoldRest}

scanCalls[pos_List, cstIn_] :=
 Module[{cst, node, tag, newlines},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  tag = node[[1]];

  newlines = Cases[tag, LeafNode[Token`Newline, _, _]];

  Function[{newline},
    FormatIssue["CallDifferentLine", "Call is on different lines.", "Formatting",
        <|Source -> newline[[3, Key[Source]]],
          AirynessLevel -> 0.4,
          CodeActions -> {
            CodeAction["DeleteTrivia", DeleteTrivia, <|Source->newline[[3, Key[Source]]]|>] }
        |>]
  ] /@ newlines
]


Attributes[scanPrefixs] = {HoldRest}

scanPrefixs[pos_List, cstIn_] :=
 Module[{cst, node, newlines, children},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  children = node[[2]];

  newlines = Cases[children, LeafNode[Token`Newline, _, _]];

  Function[{newline},
    FormatIssue["PrefixDifferentLine", "Prefix operator is on different lines.", "Formatting",
        <|Source -> newline[[3, Key[Source]]],
          AirynessLevel -> 0.4,
          CodeActions -> {
            CodeAction["DeleteTrivia", DeleteTrivia, <|Source->newline[[3, Key[Source]]]|>] }
        |>]
  ] /@ newlines
]


Attributes[scanPostfixs] = {HoldRest}

scanPostfixs[pos_List, cstIn_] :=
 Module[{cst, node, newlines, children},
  cst = cstIn;
  node = Extract[cst, {pos}][[1]];
  children = node[[2]];

  newlines = Cases[children, LeafNode[Token`Newline, _, _]];

  Function[{newline},
    FormatIssue["PostfixDifferentLine", "Postfix operator is on different lines.", "Formatting",
        <|Source -> newline[[3, Key[Source]]],
          AirynessLevel -> 0.4,
          CodeActions -> {
            CodeAction["DeleteTrivia", DeleteTrivia, <|Source->newline[[3, Key[Source]]]|>] }
        |>]
  ] /@ newlines
]






End[]


EndPackage[]
