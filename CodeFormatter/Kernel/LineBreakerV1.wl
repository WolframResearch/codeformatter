BeginPackage["CodeFormatter`LineBreakerV1`"]

breakLinesV1

$LineBreakWithinComments

$AllowSplittingTokens

Begin["`Private`"]

Needs["CodeFormatter`"]
Needs["CodeFormatter`AcceptableOperators`"] (* for isAcceptableOperator *)
Needs["CodeFormatter`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]



(*
Less common settings
*)

$LineBreakWithinComments = False

$AllowSplittingTokens = False


breakLinesV1[tokensIn_, lineWidth1_Integer, lineWidth2_Integer] :=
  Module[{tokens, lines},

    tokens = tokensIn;

    lines = Function[{split},
      (*
      we know most of the lines end with newlines
      *)
      (Most /@ Most[split]) ~Join~
      ({
        (*
        last line may not end with newline
        *)
        If[MatchQ[Last[#], LeafNode[Token`Newline, _, _] | FragmentNode[Token`Comment, $CurrentNewlineString, _]],
          Most[#]
          ,
          #
        ]&[Last[split]]
      })
    ][Split[tokens, !MatchQ[#1, LeafNode[Token`Newline, _, _] | FragmentNode[Token`Comment, $CurrentNewlineString, _]]&]];

    If[$Debug,
      Print["lines: ", lines];
    ];

    (*
    Dynamic variable $groupDepth
    *)
    Block[{$groupDepth},

      $groupDepth = 0;

      lines =
        Function[{line},
          breakLine[line, lineWidth1, lineWidth2]
        ] /@ lines;
    ];


    If[$Debug,
      Print["lines: ", lines];
    ];

    tokens = Flatten[{#, {LeafNode[Token`Newline, $CurrentNewlineString, <||>]}}& /@ lines];

    tokens
  ]

breakLinesV1[tokensIn_, Infinity, Infinity] :=
  tokensIn

breakLine[tokensIn_, lineWidth1_Integer, lineWidth2_Integer] :=
  Module[{tokens, width, tok, toSplit, takeSpecs, kTmp, toInsertAfter, firstNonWhitespaceIndex, leadingWhitespace,
    toplevel, nextTok},

    tokens = tokensIn;

    (*
    Collect leading whitespace, to be used later when inserting newlines and we want to indent the next line
    *)
    firstNonWhitespaceIndex = 1;
    leadingWhitespace = "";
    While[True,
      If[firstNonWhitespaceIndex > Length[tokens],
        Break[]
      ];
      tok = tokens[[firstNonWhitespaceIndex]];
      If[!MatchQ[tok, LeafNode[Whitespace, _, _]],
        Break[]
      ];
      leadingWhitespace = leadingWhitespace <> tok[[2]];
      firstNonWhitespaceIndex++;
    ];

    If[$Debug,
      Print["firstNonWhitespaceIndex: ", firstNonWhitespaceIndex];
      Print["leadingWhitespace: ", leadingWhitespace //InputForm];
    ];

    width = StringLength[leadingWhitespace];
    toSplit = <||>;
    toInsertAfter = {};
    Do[
      If[$Debug,
        Print["starting loop ", i];
        Print["width is: ", width];
      ];
      tok = tokens[[i]];
      nextTok = If[i < Length[tokens], tokens[[i+1]], Null];

      (*
      Manage group depth here to decide when line breaking is acceptable
      At top-level: we need to ask isAcceptableOperator[] for the token
      At not top-level: it is always ok to break after
      *)
      Switch[tok,
        (*
        Openers
        *)
        LeafNode[_?isOpener, _, _] |
        FragmentNode[_?isOpener, _, _],
          $groupDepth++;
          If[$Debug,
            Print["tok: ", tok];
            Print["$groupDepth is now: ", $groupDepth];
          ];
        ,
        (*
        Closers
        *)
        LeafNode[_?isCloser, _, _] |
        FragmentNode[_?isCloser, _, _],
          $groupDepth--;
          If[$Debug,
            Print["tok: ", tok];
            Print["$groupDepth is now: ", $groupDepth];
          ];
      ];
      toplevel = ($groupDepth == 0);

      width += StringLength[tok[[2]]];
      If[$Debug,
        Print["width is (tentatively) now 1: ", width];
      ];

      If[width <= lineWidth1,
        Continue[]
      ];

      (*
      Only loops if $AllowSplittingTokens and need to split up a token
      *)
      While[True,

        (*
        Line Breaker Algorithm V1
        Follow a simple strategy for now:
        linebreak after the first acceptable operator
        if no breaking before reaching lineWidth2, then just insert a continuation marker
        acceptable operators are:
          openers
          unambiguous prefix
          unambiguous binary
          unambiguous infix
          unambiguous ternary
        *)
        If[width <= lineWidth2,

          If[isPossiblyAcceptable[tok] &&
              (!toplevel || isAcceptableOperator[tok[[1]]]) &&
              isPossiblyAcceptableNext[nextTok]
            ,
            AppendTo[toInsertAfter, {i, leadingWhitespace <> $CurrentIndentationString}];
            width = 0;
            ,
            If[$Debug,
              Print["NOT acceptable operator; not breaking: ", tok[[1]]];
            ]
          ];

          Break[]
        ];

        (*
        width > lineWidth2
        *)

        (*
        Only add to toSplit if $AllowSplittingTokens is True
        *)
        If[TrueQ[$AllowSplittingTokens],
          If[KeyExistsQ[toSplit, i],
            toSplit[i] = Join[toSplit[i], { StringLength[tok[[2]]] - (width - lineWidth2) }]
            ,
            toSplit[i] = { StringLength[tok[[2]]] - (width - lineWidth2) }
          ];

          width = StringLength[tok[[2]]] - (StringLength[tok[[2]]] - (width - lineWidth2));
          If[$Debug,
            Print["toSplit: ", toSplit];
            Print["width is (tentatively) now 2: ", width];
          ]
          ,
          (*
          !TrueQ[$AllowSplittingTokens]
          even if not adding to toSplit, we still want to try to break after an acceptable operator
          *)
          If[isPossiblyAcceptable[tok] &&
            (!toplevel || isAcceptableOperator[tok[[1]]]) &&
            isPossiblyAcceptableNext[nextTok]
            ,
            If[$Debug,
              Print["ACCEPTABLE operator; breaking: ", tok[[1]]];
            ];
            AppendTo[toInsertAfter, {i, leadingWhitespace <> $CurrentIndentationString}];
            width = 0;
            ,
            If[$Debug,
              Print["NOT acceptable operator; not breaking: ", tok[[1]]];
            ]
          ];

          Break[]
        ]
      ] (* While[True] *)
      ,
      {i, firstNonWhitespaceIndex, Length[tokens]}
    ]; (* Do *)

    If[$Debug,
      Print["toSplit 1: ", toSplit];
      Print["toInsertAfter: ", toInsertAfter];
    ];

    (*
    Adjust toSplit as needed to not split \[Alpha] or \"
    *)
    toSplit = Association[KeyValueMap[
      Function[{key, val},
        tok = tokens[[key]];

        Which[
          StringContainsQ[tok[[2]], "\\"],
            (*
            splitting at k means to return substrings with indices {1, k} and {k+1, 20}
            *)
            key -> (Function[k,
              kTmp = k;
              Catch[
              (*
              need to loop because there may be multiple escapes to handle in a row
              *)
              While[True,
                If[$Debug,
                  Print["kTmp: ", kTmp];
                  Print["examining: ", StringTake[tok[[2]], kTmp]];
                ];
                Which[
                  (*
                  reached beginning of token
                  *)
                  kTmp == -1,
                    Throw[0]
                  ,
                  (*
                  ends with unfinished longname
                  *)
                  StringEndsQ[StringTake[tok[[2]], kTmp], RegularExpression["\\\\\\[[a-zA-Z0-9]+"]],
                    If[$Debug,
                      Print["unfinished longname"];
                    ];
                    kTmp = kTmp - StringLength[StringCases[StringTake[tok[[2]], kTmp], RegularExpression["\\\\\\[[a-zA-Z0-9]+$"]][[1]]]
                  ,
                  (*
                  ends with unfinished 2hex
                  *)
                  StringEndsQ[StringTake[tok[[2]], kTmp], RegularExpression["\\\\\\.[a-fA-F0-9]"]],
                    If[$Debug,
                      Print["unfinished 2hex"];
                    ];
                    kTmp = kTmp - StringLength[StringCases[StringTake[tok[[2]], kTmp], RegularExpression["\\\\\\.[a-fA-F0-9]$"]][[1]]]
                  ,
                  (*
                  ends with unfinished 4hex
                  *)
                  StringEndsQ[StringTake[tok[[2]], kTmp], RegularExpression["\\\\:[a-fA-F0-9]{1,3}"]],
                    If[$Debug,
                      Print["unfinished 4hex"];
                    ];
                    kTmp = kTmp - StringLength[StringCases[StringTake[tok[[2]], kTmp], RegularExpression["\\\\:[a-fA-F0-9]{1,3}$"]][[1]]]
                  ,
                  (*
                  ends with unfinished 6hex
                  *)
                  StringEndsQ[StringTake[tok[[2]], kTmp], RegularExpression["\\\\\\|[a-fA-F0-9]{1,5}"]],
                    If[$Debug,
                      Print["unfinished 6hex"];
                    ];
                    kTmp = kTmp - StringLength[StringCases[StringTake[tok[[2]], kTmp], RegularExpression["\\\\\\|[a-fA-F0-9]{1,5}$"]][[1]]]
                  ,
                  (*
                  ends with unfinished octal
                  *)
                  StringEndsQ[StringTake[tok[[2]], kTmp], RegularExpression["\\\\[0-7]{2}"]],
                    If[$Debug,
                      Print["unfinished octal"];
                    ];
                    kTmp = kTmp - StringLength[StringCases[StringTake[tok[[2]], kTmp], RegularExpression["\\\\[0-7]{2}$"]][[1]]]
                  ,
                  (*
                  ends with unfinished single escaped character
                  *)
                  StringEndsQ[StringTake[tok[[2]], kTmp], RegularExpression["\\\\."]],
                    If[$Debug,
                      Print["unfinished single character"];
                    ];
                    kTmp = kTmp - StringLength[StringCases[StringTake[tok[[2]], kTmp], RegularExpression["\\\\.$"]][[1]]]
                  ,
                  (*
                  ends with backslash
                  *)
                  StringEndsQ[StringTake[tok[[2]], kTmp], RegularExpression["\\\\"]],
                    If[$Debug,
                      Print["unfinished backslash"];
                    ];
                    kTmp = kTmp - StringLength[StringCases[StringTake[tok[[2]], kTmp], RegularExpression["\\\\$"]][[1]]]
                  ,
                  True,
                    If[$Debug,
                      Print["nothing unfinished"];
                    ];
                    Throw[kTmp]
                ]
              ]]
            ] /@ val)
          ,
          True,
            key -> val
        ] (* Which *)
      ] (* Function *)
      ,
      toSplit
    ]];

    If[$Debug,
      Print["toSplit 2: ", toSplit];
    ];

    (*
    Now split each token accordingly
    *)
    KeyValueMap[
      Function[{key, val},
        tok = tokens[[key]];
        (*
        scan for fragments such as ( * and * ) that COULD be broken but that SHOULD NOT be broken

        For aesthetics or whatever reasons

        but at least line break BEFORE this fragment
        *)
        Switch[tok,
          FragmentNode[Token`Comment, "(*", _],
            tokens[[key]] = {FragmentNode[Token`Comment, "\\" <> $CurrentNewlineString, tok[[3]]], tok}
          ,
          FragmentNode[Token`Comment, "*)", _],
            tokens[[key]] = {FragmentNode[Token`Comment, $CurrentNewlineString, tok[[3]]], tok}
          ,
          LeafNode[String, s_ /; !StringStartsQ[s, "\""], _],
            (*
            just ignore the line break for now
            FIXME: yes this is wrong
            *)
            (* tokens[[key]] = tok *)
            Null
          ,
          LeafNode[Token`Fake`ImplicitTimes, _, _],
            (*
            insert a star to work-around design issues of line continuations and implicit Times interfering with each other

            Here is an example:
            CodeFormatter`BreakLines`Private`$AllowSplittingTokens = True;
            CodeFormat["aaaaaaaaaa b", "LineWidth" -> 10]

            aaaaaaaaaa b

            and:

            aaaaaaaaaa\
             b
            
            are not the same expressions!

            aaaaaaaaaa\
             b
            
            is parsed as the symbol aaaaaaaaaab because line continuations also eat all of the leading whitespace on the nextline
            *)
            Message[CodeFormat::implicittimesaftercontinuation];
            tokens[[key]] = {FragmentNode[Token`Fake`ImplicitTimes, "\\" <> $CurrentNewlineString, tok[[3]]], FragmentNode[Token`Star, "*", <||>]}
          ,
          LeafNode[Token`LinearSyntaxBlob, _, _],
            (*
            Never split LinearSyntaxBlobs
            *)
            tokens[[key]] = {FragmentNode[Token`LinearSyntaxBlob, "\\" <> $CurrentNewlineString, tok[[3]]], tok}
          ,
          (*
          OK to split

          inside comment, so do not need to insert continuations marks
          *)
          FragmentNode[Token`Comment, _, _],
            takeSpecs = Partition[{0} ~Join~ val ~Join~ {StringLength[tok[[2]]]}, 2, 1];
            If[$Debug,
              Print["takeSpecs 1: ", takeSpecs];
            ];
            takeSpecs = Replace[takeSpecs, {
                {0, 0} -> {},
                {start_, end_} :> {start+1, end}
              }
              ,
              {1}
            ];
            If[$Debug,
              Print["takeSpecs 2: ", takeSpecs];
            ];

            If[$LineBreakWithinComments,
              tokens[[key]] = FragmentNode[tok[[1]], #, tok[[3]]]& /@ betterRiffle[StringTake[tok[[2]], takeSpecs], $CurrentNewlineString]
            ]
          ,
          (*
          OK to split
          
          No special cases, can use the original split value
          *)
          _,
            takeSpecs = Partition[{0} ~Join~ val ~Join~ {StringLength[tok[[2]]]}, 2, 1];
            If[$Debug,
              Print["takeSpecs 1: ", takeSpecs];
            ];
            takeSpecs = Replace[takeSpecs, {
                {0, 0} -> {},
                {start_, end_} :> {start+1, end}
              }
              ,
              {1}
            ];
            If[$Debug,
              Print["takeSpecs 2: ", takeSpecs];
            ];

            tokens[[key]] = FragmentNode[tok[[1]], #, tok[[3]]]& /@ betterRiffle[StringTake[tok[[2]], takeSpecs], "\\" <> $CurrentNewlineString]
        ];
      ]
      ,
      toSplit
    ];

    (*
    Now insert newlines
    *)
    Do[
      tokens[[i[[1]]]] = {tokens[[i[[1]]]], LeafNode[Token`Newline, $CurrentNewlineString, <||>]} ~Join~
        (LeafNode[Whitespace, #, <||>]& /@ Characters[i[[2]]])
      ,
      {i, toInsertAfter}
    ];

    If[$Debug,
      Print["returning from breakLine:"];
      Print["tokens: ", tokens];
      Print["$groupDepth: ", $groupDepth];
    ];

    Flatten[tokens]
  ]


(*
Not acceptable to break in the middle of a fragmented string
*)
isPossiblyAcceptable[FragmentNode[String, _, KeyValuePattern["LastFragmentInString" -> False]]] := False
isPossiblyAcceptable[LeafNode[Token`ColonColon, _, _]] := False
isPossiblyAcceptable[_] := True


(*
if the next tok is [
then do not break

having
{
  f
  []
}
is weird

This also takes care of issue of breaking up a[[]]
*)
isPossiblyAcceptableNext[LeafNode[Token`OpenSquare | Token`LongName`LeftDoubleBracket, _, _]] := False
isPossiblyAcceptableNext[LeafNode[Token`Comma, _, _]] := False
(*
prevent PostfixDifferentLine lints
related bugs: 414042
*)
isPossiblyAcceptableNext[LeafNode[Token`Amp, _, _]] := False
isPossiblyAcceptableNext[_] := True


End[]

EndPackage[]
