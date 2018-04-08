# IntervalTree map operations
using Compat

const IntervalMap{K, V} = IntervalTree{K, IntervalValue{K, V}}


function Base.setindex!(t::IntervalBTree{K, V, B}, value,
                        key::Tuple{Any, Any}) where {K, V, B}
    push!(t, V(key[1], key[2], value), true)
end


function Base.setindex!(t::IntervalBTree{K, V, B}, value,
                        key::Interval{K}) where {K, V, B}
    push!(t, V(key.first, key.last, value), true)
end


function Base.setindex!(t::IntervalBTree{K, V, B}, value,
                        first, last) where {K, V, B}
    push!(t, V(first, last, value), true)
end


function Base.getindex(t::IntervalBTree{K, V, B}, key::AbstractInterval{K}) where {K, V, B}
    return _getindex(t.root, key)
end


function _getindex(t::InternalNode{K, V, B}, key::AbstractInterval{K}) where {K, V, B}
    i = findidx(t, key)
    if 1 <= length(t) - 1 && key >= t.keys[i]
        return _getindex(t.children[i+1], key)
    else
        return _getindex(t.children[i], key)
    end
end


function _getindex(t::LeafNode{K, V, B}, key::AbstractInterval{K}) where {K, V, B}
    i = findidx(t, key)
    if 1 <= i <= length(t) && first(t.entries[i]) == first(key) &&
        last(t.entries[i]) == last(key)
        return t.entries[i]
    else
        error(KeyError((key.first, key.last)))
    end
end


function Base.get(t::IntervalBTree{K, V, B}, key::Tuple{Any, Any}, default) where {K, V, B}
    return _get(t.root, Interval{K}(key[1], key[2]), default)
end


function Base.get(t::IntervalBTree{K, V, B}, key::AbstractInterval{K}, default) where {K, V, B}
    return _get(t.root, key, default)
end


function _get(t::InternalNode{K, V, B}, key::AbstractInterval{K}, default) where {K, V, B}
    i = findidx(t, key)
    if 1 <= length(t) - 1 && key >= t.keys[i]
        return _get(t.children[i+1], key, default)
    else
        return _get(t.children[i], key, default)
    end
end


function _get(t::LeafNode{K, V, B}, key::AbstractInterval{K}, default) where {K, V, B}
    i = findidx(t, key)
    if 1 <= i <= length(t) &&
        first(t.entries[i]) == first(key) && last(t.entries[i]) == last(key)
        return t.entries[i]
    else
        return default
    end
end


function Base.get!(t::IntervalBTree{K, V, B}, key::Tuple{Any, Any}, default) where {K, V, B}
    return push!(t, V(key[1], key[2], default), true, false)
end


function Base.get!(t::IntervalBTree{K, V, B}, key::AbstractInterval{K}, default) where {K, V, B}
    return push!(t, V(key.first, key.last, default), true, false)
end


function Base.delete!(t::IntervalBTree{K, V, B}, first, last) where {K, V, B}
    return deletefirst!(t, first, last)
end


function Base.delete!(t::IntervalBTree{K, V, B}, key::Tuple{Any, Any}) where {K, V, B}
    return deletefirst!(t, key)
end


function Base.delete!(t::IntervalBTree{K, V, B}, key::Interval{K}) where {K, V, B}
    return deletefirst!(t, key)
end


struct IntervalKeyIterator{K, V, B}
    t::IntervalBTree{K, V, B}
end

Base.eltype(::Type{IntervalKeyIterator{K,V,B}}) where {K,V,B} = Interval{K}
Compat.IteratorSize(::Type{IntervalKeyIterator{K,V,B}}) where {K,V,B} = Base.SizeUnknown()


function Base.keys(t::IntervalBTree)
    return IntervalKeyIterator(t)
end


function Base.start(it::IntervalKeyIterator{K, V, B}) where {K, V, B}
    # traverse to the first leaf node
    node = it.t.root
    while !isa(node, LeafNode{K, V, B})
        node = node.children[1]
    end

    return IntervalBTreeIteratorState(node, 1)
end


function Base.next(t::IntervalKeyIterator{K, V, B},
                   state::IntervalBTreeIteratorState{K, V, B}) where {K, V, B}
    leaf = state.leaf
    key = Interval{K}(first(leaf.entries[state.i]),
                      last(leaf.entries[state.i]))
    if state.i < length(leaf)
        state = IntervalBTreeIteratorState{K, V, B}(leaf, state.i + 1)
    else
        state = IntervalBTreeIteratorState{K, V, B}(leaf.right, 1)
    end
    return key, state
end


function Base.done(t::IntervalKeyIterator{K, V, B},
                   state::IntervalBTreeIteratorState{K, V, B}) where {K, V, B}
    return (state.leaf === nothing) || isempty(notnothing(state.leaf))
end


struct IntervalValueIterator{K, V <: IntervalValue, B}
    t::IntervalBTree{K, V, B}
end

Base.eltype(::Type{IntervalValueIterator{K,V,B}}) where {K,V,B} = valtype(V)
Compat.IteratorSize(::Type{IntervalValueIterator{K,V,B}}) where {K,V,B} = Base.SizeUnknown()


function Base.values(t::IntervalBTree{K, V, B}) where {K, V<:IntervalValue, B}
    return IntervalValueIterator(t)
end


function Base.start(it::IntervalValueIterator{K, V, B}) where {K, V, B}
    # traverse to the first leaf node
    node = it.t.root
    while !isa(node, LeafNode{K, V, B})
        node = node.children[1]
    end

    return IntervalBTreeIteratorState(node, 1)
end


function Base.next(t::IntervalValueIterator{K, V, B},
                   state::IntervalBTreeIteratorState{K, V, B}) where {K, V, B}
    leaf = state.leaf
    value = leaf.entries[state.i].value
    if state.i < length(leaf)
        state = IntervalBTreeIteratorState{K, V, B}(leaf, state.i + 1)
    else
        state = IntervalBTreeIteratorState{K, V, B}(leaf.right, 1)
    end
    return value, state
end


function Base.done(t::IntervalValueIterator{K, V, B},
                   state::IntervalBTreeIteratorState{K, V, B}) where {K, V, B}
    return (state.leaf === nothing) || isempty(notnothing(state.leaf))
end
