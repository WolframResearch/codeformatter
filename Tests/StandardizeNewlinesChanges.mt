
Needs["CodeFormatter`"]

Test[
	CodeFormat["nd[k_Integer] := nd[k] = x"]
	,
"nd[k_Integer] := nd[k] =
    x"
	,
	TestID->"StandardizeNewlinesChanges-20210313-S1T3P6"
]





