BeginPackage["Format`"]

FormatFile

FormatString

FormatBytes



Begin["`Private`"]

Needs["AST`"]
Needs["AST`Abstract`"]
Needs["AST`CodeAction`"]
Needs["AST`Utils`"]




FormatFile[file_String | File[file_String]] :=
	formatFile[file]

formatFile[file_String] :=
Catch[
Module[{cst, cstAndIssues, issues, last, lastSrc, lastSrcLine, actions, newCST, str, bytes,
	actionfulIssues, actionlessIssues, srcPosMap},

	cstAndIssues = ConcreteParseFile[file, {FileNode[File, #[[1]], <||>], Cases[#[[2]], FormatIssue[___]]}&];

	If[FailureQ[cstAndIssues],
    Throw[cstAndIssues]
   ];

   {cst, issues} = cstAndIssues;

	issues = formatCST[cst, issues];

	(*
	File only
	insert trailing \n if missing
	*)
	If[!empty[ cst[[2]] ],

		last = cst[[2, -1]];

		If[!MatchQ[last, LeafNode[Token`Newline, _, _]],

			lastSrc = last[[3, Key[Source] ]];
			lastSrcLine = lastSrc[[2, 1]];

			AppendTo[issues, FormatIssue["MissingTrailingNewline", "Missing trailing newline", "Formatting", <|Source->{{lastSrcLine+1, 0}, {lastSrcLine+1, 0}}, CodeActions -> { CodeAction["Insert", InsertNodeAfter, <|Source->lastSrc, "InsertionNode"->LeafNode[Token`Newline, "\n", <||>]|>] }|>] ];
		];
	];

	actionfulIssues = Select[issues, KeyExistsQ[#[[4]], CodeActions]&];
	actionlessIssues = Complement[issues, actionfulIssues];

	Scan[(Message[FormatFile::noaction, #])&, actionlessIssues];

	actions = #[[4, Key[CodeActions], 1 ]]& /@ actionfulIssues;

	actions = SortBy[actions, -#[[3, Key[Source]]]&];


	srcPosMap = Association[Extract[cst, #] -> {#}& /@ Position[cst, {{_, _}, {_, _}}, -1, Heads -> False]];

	newCST = cst;
	Scan[(newCST = ApplyCodeAction[#, newCST, srcPosMap])&, actions];


	str = ToSourceCharacterString[newCST];

	bytes = ToCharacterCode[str, "UTF8"];

	Export[file, bytes, "Byte"]
]]





FormatString[str_String] :=
	formatString[str]

formatString[strIn_String] :=
Module[{cst, cstAndIssues, issues, actions, newCST, str,
	actionfulIssues, actionlessIssues, srcPosMap},

	cstAndIssues = ConcreteParseString[strIn, {FileNode[File, #[[1]], <||>], Cases[#[[2]], FormatIssue[___]]}&];

	If[FailureQ[cstAndIssues],
    Throw[cstAndIssues]
   ];

   {cst, issues} = cstAndIssues;

	issues = formatCST[cst, issues];

	actionfulIssues = Select[issues, KeyExistsQ[#[[4]], CodeActions]&];
	actionlessIssues = Complement[issues, actionfulIssues];

	Scan[(Message[FormatString::noaction, #])&, actionlessIssues];

	actions = #[[4, Key[CodeActions], 1 ]]& /@ actionfulIssues;

	actions = SortBy[actions, -#[[3, Key[Source]]]&];

	srcPosMap = Association[Extract[cst, #] -> {#}& /@ Position[cst, {{_, _}, {_, _}}, -1, Heads -> False]];


	newCST = cst;
	Scan[(newCST = ApplyCodeAction[#, newCST, srcPosMap])&, actions];


	If[$Debug,
		Print["newCST: ", newCST];
	];

	str = ToSourceCharacterString[newCST];

	str
]






FormatBytes[bytes_List] :=
	formatBytes[bytes]

formatBytes[bytesIn_List] :=
Catch[
Module[{cst, cstAndIssues, issues, last, lastSrc, lastSrcLine, actions, newCST, str,
	actionfulIssues, actionlessIssues, bytes, bytesOut},

	bytes = bytesIn;

	cstAndIssues = ConcreteParseBytes[bytes, {FileNode[File, #[[1]], <||>], Cases[#[[2]], FormatIssue[___]]}&];

	If[FailureQ[cstAndIssues],
    Throw[cstAndIssues]
   ];

   {cst, issues} = cstAndIssues;

	issues = formatCST[cst, issues];

	(*
	File only
	insert trailing \n if missing
	*)
	If[!empty[ cst[[2]] ],

		last = cst[[2, -1]];

		If[!MatchQ[last, LeafNode[Token`Newline, _, _]],

			lastSrc = last[[3, Key[Source] ]];
			lastSrcLine = lastSrc[[2, 1]];

			AppendTo[issues, FormatIssue["MissingTrailingNewline", "Missing trailing newline", "Formatting", <|Source->{{lastSrcLine+1, 0}, {lastSrcLine+1, 0}}, CodeActions -> { CodeAction["Insert", InsertNodeAfter, <|Source->lastSrc, "InsertionNode"->LeafNode[Token`Newline, "\n", <||>]|>] }|>] ];
		];
	];

	actionfulIssues = Select[issues, KeyExistsQ[#[[4]], CodeActions]&];
	actionlessIssues = Complement[issues, actionfulIssues];

	Scan[(Message[FormatBytes::noaction, #])&, actionlessIssues];

	actions = #[[4, Key[CodeActions], 1 ]]& /@ actionfulIssues;

	actions = SortBy[actions, -#[[3, Key[Source]]]&];

	srcPosMap = Association[Extract[cst, #] -> {#}& /@ Position[cst, {{_, _}, {_, _}}, -1, Heads -> False]];

	If[$Debug,
		Print["actions: ", actions];
	];

	
	newCST = cst;
	Scan[(newCST = ApplyCodeAction[#, newCST, srcPosMap])&, actions];


	If[$Debug,
		Print["newCST: ", newCST];
	];

	str = ToSourceCharacterString[newCST];

	If[$Debug,
		Print["str: ", str];
	];

	bytesOut = ToCharacterCode[str, "UTF8"];

	bytesOut
]]




(*
	gather concrete issues

	gather abstract issues

	
	noncontroversial issues:
	
	remove trailing whitespace at end of lines

	*)
formatCST[cst_, issuesIn_] :=
Catch[
Module[{issues, agg, ast, trailing, trailingIssues, astIssues},

	issues = issuesIn;

	agg = Aggregate[cst];

	If[FailureQ[agg],
		Throw[agg]
	];

	ast = Abstract[agg];

	astIssues = Cases[ast, FormatIssue[___], Infinity];

	trailing = trailingWhitespace[cst];

	If[$Debug,
		Print["trailing: ", trailing]
	];

	trailingIssues = FormatIssue["TrailingWhitespace", "Trailing whitespace", "Formatting", <|Source->#[[3, Key[Source] ]],
			CodeActions -> { CodeAction["Remove", DeleteNode, <|Source->#[[3, Key[Source] ]]|>] }|>]& /@ trailing;
	
	issues = issues ~Join~ astIssues ~Join~ trailingIssues;

	If[$Debug,
		Print["issues: ", issues];
	];

	(*
	str = ToSourceCharacterString[cst];

	str*)

	issues
]]





(*
Return WhiteSpace nodes that are before Newline nodes
*)
trailingWhitespace[cstIn_] :=
Catch[
Module[{cst, toks, lines, cases},

	cst = cstIn;

	(* linearize *)
	toks = Cases[cst, _LeafNode, Infinity];

	lines = Split[toks, #1[[1]] =!= Token`Newline && #1[[1]] =!= Token`LineContinuation&];

	reaped = Reap[
	Scan[sowLine, lines]
	][[2]];

	If[$Debug,
		Print["reaped: ", reaped];
	];

	If[empty[reaped],
		Throw[{}]
	];

	reaped[[1]]







	(*
	poss = Position[cst, LeafNode[Token`Newline | Token`LineContinuation, _, _]];

	(*
	Look at the last tokens, where there is not a newline
	*)
	children = cst[[2]];
	AppendTo[poss, {2, Length[children]+1}];

	If[$Debug,
		Print["poss: ", poss];
	];

	reaped = Reap[
		(	most = Most[#];
			origLast = Last[#];

			last = origLast;
			(* previous leaf *)
			While[last-1 > 0 && MatchQ[Extract[cst, Append[most, last-1]], LeafNode[Token`WhiteSpace, _, _]],
      		last--;
      	];

      	(*
			There is only whitespace on this line
      	*)
      	If[last == 0 && matched,

      	];

      	last = origLast;

			(* previous leaf *)
			last--;
			While[last > 0 && MatchQ[extracted = Extract[cst, Append[most, last]], LeafNode[Token`WhiteSpace, _, _]],
     			Sow[extracted]; 
      		last--;
      	];
      )& /@ poss
   ][[2]];

	If[$Debug,
		Print["reaped: ", reaped];
	];

	If[empty[reaped],
		Throw[{}]
	];

	reaped[[1]]
	*)
]]


sowLine[line_] :=
Module[{cases},
Switch[line,

	(* only whitespace, then \n
		also catches case of only \n

		if toplevel, then remove
	*)
	{LeafNode[Token`WhiteSpace, _, _]..., LeafNode[Token`Newline | Token`LineContinuation, _, _]},
		
		(*
		super slow:
		cases = SequenceCases[line, {ws:LeafNode[Token`WhiteSpace, _, _]..., LeafNode[Token`Newline | Token`LineContinuation, _, _]} :> ws];
		*)
		cases = Reverse[TakeWhile[Reverse[line[[1;;-2]]], MatchQ[#, LeafNode[Token`WhiteSpace, _, _]]&]];

		cases = Select[cases, toplevelQ[#, cst]&];

		Scan[Sow, cases]
	,

	(* something, then whitespace, then \n
		
		remove whitespace after something
	*)
	{___, LeafNode[Except[Token`WhiteSpace], _, _], LeafNode[Token`WhiteSpace, _, _]..., LeafNode[Token`Newline | Token`LineContinuation, _, _]},
		
		(*
		super slow:
		cases = SequenceCases[line, {___, LeafNode[Except[Token`WhiteSpace], _, _], ws:LeafNode[Token`WhiteSpace, _, _]..., LeafNode[Token`Newline | Token`LineContinuation, _, _]} :> ws];
		*)
		cases = Reverse[TakeWhile[Reverse[line[[1;;-2]]], MatchQ[#, LeafNode[Token`WhiteSpace, _, _]]&]];

		Scan[Sow, cases]
	,

	(* only whitespace, at EOF

		if toplevel, then remove
	*)
	{LeafNode[Token`WhiteSpace, _, _]...},
		
		(*
		super slow:
		cases = SequenceCases[line, {ws:LeafNode[Token`WhiteSpace, _, _]...} :> ws];
		*)
		cases = Reverse[TakeWhile[Reverse[line[[1;;-1]]], MatchQ[#, LeafNode[Token`WhiteSpace, _, _]]&]];

		cases = Select[cases, toplevelQ[#, cst]&];

		Scan[Sow, cases]
	,

	(* something, then whitespace, at EOF

		remove whitespace after something
	*)
	{___, LeafNode[Except[Token`WhiteSpace], _, _], LeafNode[Token`WhiteSpace, _, _]...},
		
		(*
		super slow:
		cases = SequenceCases[line, {___, LeafNode[Except[Token`WhiteSpace], _, _], ws:LeafNode[Token`WhiteSpace, _, _]...} :> ws];
		*)
		cases = Reverse[TakeWhile[Reverse[line[[1;;-1]]], MatchQ[#, LeafNode[Token`WhiteSpace, _, _]]&]];

		Scan[Sow, cases]
]]


toplevelQ[tok_, cst_] := MatchQ[FirstPosition[cst, tok], {2, _}]







End[]

EndPackage[]
