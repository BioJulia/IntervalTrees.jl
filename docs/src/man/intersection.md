# Intersection

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