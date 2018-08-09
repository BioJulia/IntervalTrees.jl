# Creating IntervalTrees & inserting intervals

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
