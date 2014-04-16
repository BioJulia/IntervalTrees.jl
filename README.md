
**This is a work in proress interval tree implementation. Not yet ready for use.**

This is a B+-tree variant used to map intervals (any tuple `(a, b)` over an
ordered type where `a <= b`) to arbitrary data supporting fast lookups,
deletions, and intersection tests.

## Rationale 

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

