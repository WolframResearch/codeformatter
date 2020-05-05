BeginPackage["CodeFormatter`Utils`"]


replaceTabs

tabReplacementFunc


Begin["`Private`"]

(*
Memoizing function that returns the number of spaces that a tab should be replaced with

tabReplacementFunc[1, 4] => "    "
tabReplacementFunc[2, 4] => "   "
tabReplacementFunc[3, 4] => "  "
tabReplacementFunc[4, 4] => " "
tabReplacementFunc[5, 4] => "    "
tabReplacementFunc[6, 4] => "   "
tabReplacementFunc[7, 4] => "  "
tabReplacementFunc[8, 4] => " "
tabReplacementFunc[9, 4] => "    "

*)
tabReplacementFunc[col_Integer, tabWidth_Integer] :=
  tabReplacementFunc[col, tabWidth] =
  StringJoin[Table[" ", Mod[1 - col, tabWidth, 1]]]


replaceTabs[str_String, startingColumn_Integer, newline_String, tabWidth_Integer] :=
Module[{lines},
  lines = StringSplit[str, newline, All];
  (*
  Pad the first line with the correct number of characters from its origin
  *)
  lines[[1]] = StringJoin[Table["!", startingColumn - 1], lines[[1]]];
  lines = Map[
    Function[{line},
      (*
      for each line, accumulate a string by replacing each tab with its equivalent spaces,
      working from left to right
      *)
      NestWhile[
        Function[{lineAccum},
          With[{pos = StringPosition[lineAccum, "\t"][[1, 1]]},
            StringReplacePart[lineAccum, tabReplacementFunc[pos, tabWidth], {pos, pos}]]
        ]
        ,
        line
        ,
        StringContainsQ[#, "\t"]&
      ]
    ]
    ,
    lines
  ];
  (*
  Remove padding
  *)
  lines[[1]] = StringDrop[lines[[1]], startingColumn - 1];
  StringJoin[Riffle[lines, newline]]
]


End[]

EndPackage[]
