# Iteration

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