# Line Breaker v2


a very simple line breaking algorithm

tree structure

not context sensitive

bottom-up

when the limit is hit, then the node reformats with "Multiline" -> True

this may now shorten the lines in the node and the limit may not be hit any more, but oh well, we do not check again for that node

going to "Multiline" -> True is "sticky", it is "poison", once it is turned on, it does not turn off

this is version 2 of the line breaking algorithm









## current limitations of LineBreakerV2

```
"resourcePublisherNameSpaceFreeQ[name_String] :=
    With[
        {
            ns
            =
            DeleteMissing[publisherResourceNameSpace /@ allPublisherInfo[][\"Publishers\"]]
        }
        ,
        !MatchQ[name, Alternatives @@ ns]
    ]"
```

line ends up being 91 characters long because:

`DeleteMissing[publisherResourceNameSpace /@ allPublisherInfo[]["Publishers"]]` = 77 characters, so 1 line

`ns = DeleteMissing[publisherResourceNameSpace /@ allPublisherInfo[]["Publishers"]]` = 82 characters, so multiline

IndentationNode[Block, {the above code}] = 81 characters

nothing ever forces first line to re-indent






