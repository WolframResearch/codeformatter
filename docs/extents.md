# Extents


An extent is:

{width, height, leadingWidth, trailingWidth}

width = width of bounding box
height = height of bounding box
leadingWidth = width of first line
trailingWidth = width of last line

Some examples:

123

extent is {3, 1, 3, 3}


If[a,
    b
    ,
    c
]

extent is {5, 5, 5, 1}



Extents form a monoid

There is an identity: {0, 1, 0, 0}

And there is an associative binary operation: combine










