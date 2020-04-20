# IntervalTree map operations

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
        error(KeyError((first(key), last(key))))
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
    return push!(t, V(first(key), last(key), default), true, false)
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
Base.IteratorSize(::Type{IntervalKeyIterator{K,V,B}}) where {K,V,B} = Base.SizeUnknown()


function Base.keys(t::IntervalBTree)
    return IntervalKeyIterator(t)
end

function Base.iterate(
        it::IntervalKeyIterator{K, V, B},
        state::IntervalBTreeIteratorState{K, V, B}=iterinitstate(it)) where {K, V, B}
    if state.leaf === nothing || isempty(notnothing(state.leaf))
        return nothing
    end
    leaf = state.leaf
    key = Interval{K}(first(leaf.entries[state.i]), last(leaf.entries[state.i]))
    if state.i < length(leaf)
        state = IntervalBTreeIteratorState{K, V, B}(leaf, state.i + 1)
    else
        state = IntervalBTreeIteratorState{K, V, B}(leaf.right, 1)
    end
    return key, state
end

function iterinitstate(it::IntervalKeyIterator{K, V, B}) where {K, V, B}
    # traverse to the first leaf node
    node = it.t.root
    while !isa(node, LeafNode{K, V, B})
        node = node.children[1]
    end
    return IntervalBTreeIteratorState(node, 1)
end


struct IntervalValueIterator{K, V <: IntervalValue, B}
    t::IntervalBTree{K, V, B}
end

Base.eltype(::Type{IntervalValueIterator{K,V,B}}) where {K,V,B} = valtype(V)
Base.IteratorSize(::Type{IntervalValueIterator{K,V,B}}) where {K,V,B} = Base.SizeUnknown()


function Base.values(t::IntervalBTree{K, V, B}) where {K, V<:IntervalValue, B}
    return IntervalValueIterator(t)
end

function Base.iterate(
        it::IntervalValueIterator{K, V, B},
        state::IntervalBTreeIteratorState{K, V, B}=iterinitstate(it)) where {K, V, B}
    if state.leaf === nothing || isempty(notnothing(state.leaf))
        return nothing
    end
    leaf = state.leaf
    value = leaf.entries[state.i].value
    if state.i < length(leaf)
        state = IntervalBTreeIteratorState{K, V, B}(leaf, state.i + 1)
    else
        state = IntervalBTreeIteratorState{K, V, B}(leaf.right, 1)
    end
    return value, state
end

function iterinitstate(it::IntervalValueIterator{K, V, B}) where {K, V, B}
    # traverse to the first leaf node
    node = it.t.root
    while !isa(node, LeafNode{K, V, B})
        node = node.children[1]
    end
    return IntervalBTreeIteratorState(node, 1)
end
