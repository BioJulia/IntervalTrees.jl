
# IntervalTrees

[![Build Status](https://api.travis-ci.org/BioJulia/IntervalTrees.jl.svg?branch=master)](https://travis-ci.org/BioJulia/IntervalTrees.jl) [![Coverage Status](https://img.shields.io/coveralls/BioJulia/IntervalTrees.jl.svg)](https://coveralls.io/r/BioJulia/IntervalTrees.jl?branch=master)

## API

IntervalTrees exports one type: `IntervalTree{K, V}`.  It implements an
associative container mapping `(K, K)` pairs to to values of type `V`.  `K` may
be any ordered type, but only pairs `(a, b)` where `a ≤ b` can be stored.

Intervals in this package are always treated as end-inclusive, similar to the
Julia `Range` type.

### Types

`IntervalTrees` exports an `IntervalTree{K, V}`, parameterized by a key type `K`
and a value type `V`. An empty `IntervalTree` is initialized simply as:

```julia
xs = IntervalTree{Int, Int}()
```


### Standard Dictionary Operations

`IntervalTree` implements all the standard dictionary operations. You can use it
as an efficient way to map `(K, K)` tuples to values.


```julia
using IntervalTrees

# Create an interval tree mapping (Int, Int) intervals to Strings.
x = IntervalTree{Int, String}()

# Insert values
x[(1,100)] = "Low"
x[(101,1000)] = "Medium"
x[(1001,10000)] = "High"

# Search for values
println(xs[(1001,10000)]) # prints "High"

# Get a value, returning a default value if not found
println(get(xs, (10001, 100000), "Not found")) # prints "Not found"

# Set a value if it's not already present
println(set(xs, (10001, 100000), "Not found"))

# Delete values
delete!(xs, (1,100))

```


### Intersection

The primary thing an `IntervalTree` offers over a `Dict` is the ability to efficiently
find intersections. `IntervalTrees` supports searching and iterating over
intersections between two trees or between a tree and a single interval.

**intersect(t::IntervalTree, query::(Any, Any))**: Return an iterator over every
`(key, value)` pair in `t` that intersects `query`.


**intersect(t1::IntervalTree, t2::IntervalTree)**: Return an iterator over every
pair of intersecting entries `((key1, value1), (key2, value2))`, where `(key1,
value1)` is in `t1` and `(key2, value2)` is in `t2`.



## Algorithms

Multiple data structures are refered to as "interval trees". What's implemented
here is the data structure described in the [Cormen, et al. "Algorithms" book](https://en.wikipedia.org/wiki/Introduction_to_Algorithms), or
what's refered to as an [augmented
tree](http://en.wikipedia.org/wiki/Interval_tree#Augmented_tree) in the
wikipedia article. This sort of data structure is just an balanced search tree
augmented with a field to keep track of the maximum interval end point in that
node's subtree.

Many operations over two or more sets of intervals can be most efficiently
implemented by jointly iterating over the sets in order. For example, finding
all the intersecting intervals in two sets S and T can be implemented similarly
to the merge function in mergesort in O(n+m) time.

Thus a general purpose data structure should be optimized for fast in-order
iteration while efficiently supporting other operations like insertion,
deletion, and single intersection tests. A B+-tree is nicely suited to the task.
Since all the intervals and values are stored in contiguous memory in the leaf
nodes, and the leaf nodes augmented with sibling pointers, in-order traversal is
exceedingly efficient compared to other balanced search trees, while other
operations are comparable in performance.

