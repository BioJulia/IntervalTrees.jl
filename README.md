
# IntervalTrees

**This is a work in progress interval tree implementation. Not yet ready for use.**

[![Build Status](https://api.travis-ci.org/BioJulia/IntervalTrees.jl.svg?branch=master)](https://travis-ci.org/BioJulia/IntervalTrees.jl)

## API

IntervalTrees exports one type: `IntervalTree{K, V}`.  It implement a
associative container mapping `(K, K)` pairs to to values of type `V`.  `K` may
be any ordered type, but only pairs `(a, b)` where `a ≤ b` can be stored.

Besides supporting standard dictionary operations (such as `S[(a,b)] = c`,
`delete!(S, (a,b))`, or `haskey(S, (a,b))`), IntervalTrees can efficiently
perform intersection, and other interval specific operations. The API for
intersections, union, extensions, etc is still being defined, and this README
will be expanded when completed.


## Algorithms

This is a B+-tree variant used to map intervals (any tuple `(a, b)` over an
ordered type where `a <= b`) to arbitrary data supporting fast lookups,
deletions, and intersection tests.

Many operations over two or more sets of intervals can be most efficiently
implemented by jointly iterating over the sets in order. For example, finding
all the intersecting intervals in two sets S and T can be implemented similarly
to the merge function in mergesort in O(n+m) time.

Thus a general purpose-data structure should be optimized for fast in-order
iteration while efficiently supporting other operations like insertion,
deletion, and single intersection tests. A B+-tree is nicely suited to the task.
Since all the intervals and values are stored in contiguous memory in the leaf
nodes, and the leaf-nodes augmented with sibling pointers, in-order traversal is
exceedingly efficient compared to other balanced search trees, while other
operations are comparable in performance.

