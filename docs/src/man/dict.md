# Standard Dictionary Operations

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
