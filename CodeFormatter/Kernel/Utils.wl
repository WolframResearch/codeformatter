BeginPackage["CodeFormatter`Utils`"]

trivia
ws
nl
comment
matchNewlineQ
matchCommentFragmentNewlineQ


lexSort


firstToken
lastToken


firstLeafNode
lastLeafNode


isOpener
isCloser


insertNecessarySpaces


betterRiffle

surround


Begin["`Private`"]

Needs["CodeFormatter`"]
Needs["CodeParser`"]


trivia = LeafNode[Whitespace | Token`Boxes`MultiWhitespace | Token`Comment | Token`Newline, _, _]

ws = LeafNode[Whitespace | Token`Boxes`MultiWhitespace, _, _]

nl = LeafNode[Token`Newline, _, _]

comment = LeafNode[Token`Comment, _, _]

matchNewlineQ = MatchQ[nl]

matchCommentFragmentNewlineQ := MatchQ[FragmentNode[Token`Comment, $CurrentNewline, _]]



(*
Define lexical ordering to use for lists

The default Wolfram Language ordering is not the same as common lexical ordering
*)
lexOrderingForLists[{}, {}] := 0
lexOrderingForLists[{}, b_] := 1
lexOrderingForLists[a_, {}] := -1
lexOrderingForLists[a_, b_] :=
  Order[Take[a, 1], Take[b, 1]] /. 
    0 :> lexOrderingForLists[Drop[a, 1], Drop[b, 1]]

(*

Unreported bug in v11.0:

In[77]:= Sort[{{1, 1}, {1, 3}, {1, 2}}, lexOrderingForLists]

Out[77]= {{1, 1}, {1, 3}, {1, 2}}

Fixed in 11.1

checkUnreportedSortBug1[] sets the flag $WorkaroundUnreportedSortBug1 to True if we still need to workaround unreported Sort bug 1
*)
checkUnreportedSortBug1[] :=
Module[{res},
  res = Sort[{{1, 1}, {1, 3}, {1, 2}}, lexOrderingForLists];
  Switch[res,
    {{1, 1}, {1, 3}, {1, 2}},
      True
    ,
    {{1, 1}, {1, 2}, {1, 3}},
      False
  ]
]



$WorkaroundUnreportedSortBug1 = checkUnreportedSortBug1[]


(*
Yes, this slower than it needs to be
But it is simple and reliable
*)
bubbleLexSort[listIn_] :=
Module[{list, len, tmp},
  list = listIn;
  len = Length[list];
  Do[
    If[lexOrderingForLists[list[[i]], list[[j]]] == -1,
      tmp = list[[i]]; 
      list[[i]] = list[[j]];
      list[[j]] = tmp
    ];
    ,
    {i, 1, len}
    ,
    {j, i, len}
  ];
  list
]


If[$WorkaroundUnreportedSortBug1,
  lexSort = bubbleLexSort
  ,
  (*
  TODO: v12.0 introduced SortBy[list, f, p]
  when targeting v12.0 as a minimum, then can use SortBy[list, ToCharacterCode, lexOrderingForLists]
  *)
  lexSort = Sort[#, lexOrderingForLists]&
]




firstToken[LeafNode[tok_, _, _]] := tok

firstToken[CallNode[{first_, ___}, _, _]] := firstToken[first]

firstToken[_[_, {first_, ___}, _]] := firstToken[first]



lastToken[LeafNode[tok_, _, _]] := tok

lastToken[_[_, {___, last_}, _]] := lastToken[last]



firstLeafNode[leaf:LeafNode[_, _, _]] := 

firstLeafNode[CallNode[{first_, ___}, _, _]] := firstLeafNode[first]

firstLeafNode[_[_, {first_, ___}, _]] := firstLeafNode[first]



lastLeafNode[leaf:LeafNode[_, _, _]] := leaf

lastLeafNode[_[_, {___, last_}, _]] := lastLeafNode[last]



isOpener[Token`LessBar] = True
isOpener[Token`OpenCurly] = True
isOpener[Token`OpenParen] = True
isOpener[Token`OpenSquare] = True
isOpener[Token`LongName`OpenCurlyDoubleQuote] = True
isOpener[Token`LongName`OpenCurlyQuote] = True
isOpener[Token`LongName`LeftAngleBracket] = True
isOpener[Token`LongName`LeftAssociation] = True
isOpener[Token`LongName`LeftBracketingBar] = True
isOpener[Token`LongName`LeftCeiling] = True
isOpener[Token`LongName`LeftDoubleBracket] = True
isOpener[Token`LongName`LeftDoubleBracketingBar] = True
isOpener[Token`LongName`LeftFloor] = True
isOpener[Token`LinearSyntax`OpenParen] = True

isOpener[_] = False


isCloser[Token`BarGreater] = True
isCloser[Token`CloseCurly] = True
isCloser[Token`CloseParen] = True
isCloser[Token`CloseSquare] = True
isCloser[Token`LongName`CloseCurlyDoubleQuote] = True
isCloser[Token`LongName`CloseCurlyQuote] = True
isCloser[Token`LongName`RightAngleBracket] = True
isCloser[Token`LongName`RightAssociation] = True
isCloser[Token`LongName`RightBracketingBar] = True
isCloser[Token`LongName`RightCeiling] = True
isCloser[Token`LongName`RightDoubleBracket] = True
isCloser[Token`LongName`RightDoubleBracketingBar] = True
isCloser[Token`LongName`RightFloor] = True
isCloser[Token`LinearSyntax`CloseParen] = True

isCloser[_] = False



(*
Used by CodeMinifier also
*)

insertNecessarySpaces[tokensIn_] :=
  Module[{poss, tokens, toInsert, poss1},

    If[$Debug,
      Print["insertNecessarySpaces: ", tokensIn];
    ];

    tokens = tokensIn;
    toInsert = {};

    (*
    Insert space for  1.2` +3
    *)
    poss = Position[tokens, LeafNode[Real, str_ /; StringEndsQ[str, "`"], _]];
    poss = Select[poss, (#[[1]] < Length[tokens] && MatchQ[tokens[[#[[1]]+1]], LeafNode[Token`Plus | Token`Minus, _, _]])&];
    Scan[(AppendTo[toInsert, #+1])&, poss];

    (*
    Insert space for  a. 5
    *)
    poss = Position[tokens, LeafNode[Token`Dot | Token`DotDot | Token`DotDotDot, _, _]];
    poss = Select[poss, (#[[1]] < Length[tokens] && MatchQ[tokens[[#[[1]]+1]], LeafNode[Integer | Real | Rational, _, _]])&];
    Scan[(AppendTo[toInsert, #+1])&, poss];

    (*
    Insert space for  2 .a
    *)
    poss = Position[tokens, LeafNode[Integer | Real | Rational, _, _]];
    poss = Select[poss, (#[[1]] < Length[tokens] && MatchQ[tokens[[#[[1]]+1]], LeafNode[Token`Dot | Token`DotDot | Token`DotDotDot, _, _]])&];
    Scan[(AppendTo[toInsert, #+1])&, poss];

    (*
    Insert space for  a_ .b

    This used to also check for UnderDot, as in:  a_. .b

    But this behavior was fixed in 12.2 and a_. .b may now be minified to a_..b
    *)
    poss = Position[tokens, LeafNode[Token`Under, _, _]];
    poss = Select[poss, (#[[1]] < Length[tokens] && MatchQ[tokens[[#[[1]]+1]], LeafNode[Token`Dot | Token`DotDot | Token`DotDotDot, _, _]])&];
    Scan[(AppendTo[toInsert, #+1])&, poss];

    (*
    Insert space for a_..b
    *)
    poss = Position[tokens, LeafNode[Token`UnderDot, _, _]];
    poss = Select[poss, (#[[1]] < Length[tokens] && MatchQ[tokens[[#[[1]]+1]], LeafNode[Token`Dot | Token`DotDot | Token`DotDotDot, _, _]])&];
    Scan[(AppendTo[toInsert, #+1])&, poss];

    (*
    Insert space for  a - -b
    *)
    poss = Position[tokens, LeafNode[Token`Minus, _, _]];
    poss1 = Select[poss, (#[[1]] < Length[tokens] && MatchQ[tokens[[#[[1]]+1]], LeafNode[Token`Minus | Token`MinusMinus, _, _]])&];
    Scan[(AppendTo[toInsert, #+1])&, poss1];
    (*
    Insert space for  a - -1
    *)
    poss1 = Select[poss, (#[[1]] < Length[tokens] && MatchQ[tokens[[#[[1]]+1]], LeafNode[Integer | Real | Rational, str_ /; StringStartsQ[str, "-"], _]])&];
    Scan[(AppendTo[toInsert, #+1])&, poss1];

    (*
    Insert space for  a! !
    *)
    poss = Position[tokens, LeafNode[Token`Bang, _, _]];
    poss1 = Select[poss, (#[[1]] < Length[tokens] && MatchQ[tokens[[#[[1]]+1]], LeafNode[Token`Bang | Token`BangBang | Token`Equal | Token`BangEqual | Token`EqualEqual | Token`EqualEqualEqual, _, _]])&];
    Scan[(AppendTo[toInsert, #+1])&, poss1];

    (*
    Insert space for  a + +b
    *)
    poss = Position[tokens, LeafNode[Token`Plus, _, _]];
    poss = Select[poss, (#[[1]] < Length[tokens] && MatchQ[tokens[[#[[1]]+1]], LeafNode[Token`Plus | Token`PlusPlus, _, _]])&];
    Scan[(AppendTo[toInsert, #+1])&, poss];

    (*
    Insert space for  a & & + b
    *)
    poss = Position[tokens, LeafNode[Token`Amp, _, _]];
    poss = Select[poss, (#[[1]] < Length[tokens] && MatchQ[tokens[[#[[1]]+1]], LeafNode[Token`Amp, _, _]])&];
    Scan[(AppendTo[toInsert, #+1])&, poss];

    (*
    Insert space for  a < << b
    *)
    poss = Position[tokens, LeafNode[Token`Less, _, _]];
    poss = Select[poss, (#[[1]] < Length[tokens] && MatchQ[tokens[[#[[1]]+1]], LeafNode[Token`LessLess, _, _]])&];
    Scan[(AppendTo[toInsert, #+1])&, poss];

    (*
    Insert space for  a >> b !

    tutorial/OperatorInputForms
    File Names
    *)
    poss = Position[tokens, LeafNode[String, str_ /; !StringStartsQ[str, "\""], _]];
    poss = Select[poss, (#[[1]] < Length[tokens] && MatchQ[tokens[[#[[1]]+1]], LeafNode[Token`Bang, _, _]])&];
    Scan[(AppendTo[toInsert, #+1])&, poss];

    toInsert = ReverseSort[toInsert];

    tokens = Fold[Function[{toks, pos}, Insert[toks, LeafNode[Whitespace, " ", <||>], pos]], tokens, toInsert];


    (*
    Now replace implicit Times with a space

    If the implicit Times is at the end of a line, then insert actual * character
    *)
    poss = Position[tokens, LeafNode[Token`Fake`ImplicitTimes, _, _]];
    poss1 = Select[poss, (#[[1]] < Length[tokens] && MatchQ[tokens[[#[[1]]+1]], LeafNode[Token`Newline, _, _]])&];
    poss2 = Complement[poss, poss1];

    tokens = ReplacePart[tokens, poss1 -> LeafNode[Token`Star, "*", <||>]];
    tokens = ReplacePart[tokens, poss2 -> LeafNode[Token`Fake`ImplicitTimes, " ", <||>]];

    tokens
  ]



(*
It is convenient to have a function that does:

betterRiffle[{1}, {2}] => {1}

Unfortunately, Riffle does this:
Riffle[{1}, {2}] => {1, 2}

So introduce betterRiffle
*)
betterRiffle[{a_}, _] := {a}

betterRiffle[a_, b_] := Riffle[a, b]



(*

This is useful for surrounding comments with newlines
But if there are no comments, then no newlines are injected

surround[{}, x] => {}
surround[{c1, c2, c3}, x] => {x, c1, x, c2, x, c3, x}
*)
surround[{}, _] := {}

surround[a_, b_] := Riffle[a, b, {1, -1, 2}]


End[]

EndPackage[]
