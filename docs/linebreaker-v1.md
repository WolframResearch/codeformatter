# Line Breaker v1


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



## limitations

Most limitations are related to the greedy algorithm that is used.

### no back-tracking before safety margin

LineBreakerV1 does not back-track looking for a good place to break.

For example
```
f["aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" -> "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"]
```

The `"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"` extends past the 80 column limit, but it also starts before 70 column safety margin.

So LineBreakerV1 is not aware of it.




### no preference between multiple choices to break

This is a formatted example:

```
f["aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
	 -> "bbb"]
```

but it would be better if it formatted as:

```
f["aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ->
	"bbb"]
```

But `->` occurs first, so it is broken before the `->`






