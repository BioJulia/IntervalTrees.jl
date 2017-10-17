
# IntervalTrees

[![Build Status](https://api.travis-ci.org/BioJulia/IntervalTrees.jl.svg?branch=master)](https://travis-ci.org/BioJulia/IntervalTrees.jl)
[![codecov.io](http://codecov.io/github/BioJulia/IntervalTrees.jl/coverage.svg?branch=master)](http://codecov.io/github/BioJulia/IntervalTrees.jl?branch=master)

## API

IntervalTrees exports one type: `IntervalTree{K, V}`.  It implements an
associative container mapping `(K, K)` pairs to to values of type `V`.  `K` may
be any ordered type, but only pairs `(a, b)` where `a ≤ b` can be stored.

Intervals in this package are always treated as end-inclusive, similar to the
Julia `Range` type.

### Types

`IntervalTrees` exports an abstract type `AbstractInterval{K}`. Types deriving
from it are expected to implement `first` and `last` methods that return the
values of type `K` giving the inclusive range of the interval.

There are also basic interval type provided:
```julia
struct Interval{T} <: AbstractInterval{T}
    first::T
    last::T
end

struct IntervalValue{K, V} <: AbstractInterval{K}
    first::K
    last::K
    value::V
end
```

The basic data structure implemented is `IntervalTree{K, V}`, which stores
intervals of type `V`, that have start and end positions of type `K`.

`IntervalMap{K, V}` is a typealias for `IntervalTree{K, IntervalValue{K, V}}`
to simplify associating data of type `V` with intervals.


### Insertion and Initialization

New intervals can be added to an `IntervalTree` with the `push!` function.

```julia
xs = IntervalTree{Int, Interval{Int}}()
push!(xs, Interval{Int}(500, 1000))
```

A more efficient means of building the data structure by bulk insertion.
If the intervals are knows up front and provided in a sorted array, an
`IntervalTree` can be built extremely efficiently.

```julia
intervals = Interval{Int}[]
# construct a large array of intervals...

sort!(intervals)
xs = IntervalTree{Int, Interval{Int}}(intervals)
```

### Standard Dictionary Operations

`IntervalTree` implements all the standard dictionary operations. You can use it
as an efficient way to map `(K, K)` tuples to values.


```julia
using IntervalTrees

# Create an interval tree mapping (Int, Int) intervals to Strings.
xs = IntervalMap{Int, String}()

# Insert values
xs[(1,100)] = "Low"
xs[(101,1000)] = "Medium"
xs[(1001,10000)] = "High"

# Search for values
println(xs[(1001,10000)]) # prints "High"

# Get a value, returning a default value if not found
println(get(xs, (10001, 100000), "Not found")) # prints "Not found"

# Set a value if it's not already present
println(set(xs, (10001, 100000), "Not found"))

# Delete values
delete!(xs, (1,100))

```

### Iteration

As with dictionaries, key/value pairs can be iterated through efficiently.

```julia
    for x in xs
        println("Interval $(x.first), $(x.last) has value $(x.value)")
    end
```

Some other iteration functions are provided:

**from(t::IntervalTree, query)**: Return an iterator thats iterates through every
key/value pair with an end position >= to query.

**keys(t::IntervalTree)**: Return an iterator that iterates through every
interval key in the tree.

**values(t::IntervalTree)**: Return an iterator that iterates through every
value in the tree.


### Intersection

The primary thing an `IntervalTree` offers over a `Dict` is the ability to efficiently
find intersections. `IntervalTrees` supports searching and iterating over
intersections between two trees or between a tree and a single interval.

**intersect(t::IntervalTree, query::(Any, Any))**: Return an iterator over every
interval in `t` that intersects `query`.


**intersect(t1::IntervalTree, t2::IntervalTree)**: Return an iterator over every
pair of intersecting entries `(interval1, interval2)`, where `interval1` is
in `t1` and `interval2` is in `t2`.


**hasintersection(t::IntervalTree, position)**: Return true if `position`
intersects some interval in `t`.


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
