```@meta
CurrentModule = IntervalTrees
```

# Intervals & IntervalTrees

`IntervalTrees` exports an abstract type `AbstractInterval{K}`, in addition
to two basic concrete interval types.

```@docs
AbstractInterval
Interval
IntervalValue
```

Intervals in this package are always treated as end-inclusive, similar to the
Julia `Range` type.


### IntervalTrees

The basic data structure implemented in this package is `IntervalTree{K, V}`,
which stores intervals of type `V`, that have start and end positions of type `K`.

`IntervalMap{K, V}` is a typealias for `IntervalTree{K, IntervalValue{K, V}}`
to simplify associating data of type `V` with intervals.

Multiple data structures are refered to as "interval trees". What's implemented
here is the data structure described in the [Cormen, et al. "Algorithms"
book](https://en.wikipedia.org/wiki/Introduction_to_Algorithms), or what's
refered to as an [augmented
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