# IntervalTree map operations

typealias IntervalMap{K, V} IntervalTree{K, IntervalValue{K, V}}


function Base.setindex!{K, V, B}(t::IntervalBTree{K, V, B}, value,
                                 key::(@compat Tuple{Any, Any}))
    push!(t, V(key[1], key[2], value), true)
end


function Base.setindex!{K, V, B}(t::IntervalBTree{K, V, B}, value,
                                 key::Interval{K})
    push!(t, V(key.first, key.last, value), true)
end


function Base.setindex!{K, V, B}(t::IntervalBTree{K, V, B}, value,
                                 first, last)
    push!(t, V(first, last, value), true)
end


function Base.getindex{K, V, B}(t::IntervalBTree{K, V, B}, key::AbstractInterval{K})
    return _getindex(t.root, key)
end


function _getindex{K, V, B}(t::InternalNode{K, V, B}, key::AbstractInterval{K})
    i = findidx(t, key)
    if 1 <= length(t) - 1 && key >= t.keys[i]
        return _getindex(t.children[i+1], key)
    else
        return _getindex(t.children[i], key)
    end
end


function _getindex{K, V, B}(t::LeafNode{K, V, B}, key::AbstractInterval{K})
    i = findidx(t, key)
    if 1 <= i <= length(t) && first(t.entries[i]) == first(key) &&
        last(t.entries[i]) == last(key)
        return t.values[i]
    else
        error(KeyError((key.a, key.b)))
    end
end


function Base.get{K, V, B}(t::IntervalBTree{K, V, B}, key::(@compat Tuple{Any, Any}),
                           default)
    return _get(t.root, Interval{K}(key[1], key[2]), default)
end


function Base.get{K, V, B}(t::IntervalBTree{K, V, B}, key::AbstractInterval{K}, default)
    return _get(t.root, key, default)
end


function _get{K, V, B}(t::InternalNode{K, V, B}, key::AbstractInterval{K}, default)
    i = findidx(t, key)
    if 1 <= length(t) - 1 && key >= t.keys[i]
        return _get(t.children[i+1], key, default)
    else
        return _get(t.children[i], key, default)
    end
end


function _get{K, V, B}(t::LeafNode{K, V, B}, key::AbstractInterval{K}, default)
    i = findidx(t, key)
    if 1 <= i <= length(t) &&
        first(t.entries[i]) == first(key) && last(t.entries[i]) == last(key)
        return t.entries[i]
    else
        return default
    end
end


function Base.get!{K, V, B}(t::IntervalBTree{K, V, B},
                            key::(@compat Tuple{Any, Any}), default)
    return push!(t, V(key[1], key[2], default), true, false)
end


function Base.get!{K, V, B}(t::IntervalBTree{K, V, B}, key::AbstractInterval{K}, default)
    return push!(t, V(key.first, key.last, default), true, false)
end


function Base.delete!{K, V, B}(t::IntervalBTree{K, V, B}, first, last)
    return deletefirst!(t, first, last)
end


function Base.delete!{K, V, B}(t::IntervalBTree{K, V, B}, key::(@compat Tuple{Any, Any}))
    return deletefirst!(t, key)
end


function Base.delete!{K, V, B}(t::IntervalBTree{K, V, B}, key::Interval{K})
    return deletefirst!(t, key)
end


immutable IntervalKeyIterator{K, V, B}
    t::IntervalBTree{K, V, B}
end


function Base.keys(t::IntervalBTree)
    return IntervalKeyIterator(t)
end


function Base.start{K, V, B}(it::IntervalKeyIterator{K, V, B})
    # traverse to the first leaf node
    node = it.t.root
    while !isa(node, LeafNode{K, V, B})
        node = node.children[1]
    end

    return IntervalBTreeIteratorState(Nullable(node), 1)
end


function Base.next{K, V, B}(t::IntervalKeyIterator{K, V, B},
                            state::IntervalBTreeIteratorState{K, V, B})
    leaf = get(state.leaf)
    key = Interval{K}(first(leaf.entries[state.i]),
                      last(leaf.entries[state.i]))
    if state.i < length(leaf)
        state = IntervalBTreeIteratorState{K, V, B}(leaf, state.i + 1)
    else
        state = IntervalBTreeIteratorState{K, V, B}(leaf.right, 1)
    end
    return key, state
end


function Base.done{K, V, B}(t::IntervalKeyIterator{K, V, B},
                            state::IntervalBTreeIteratorState{K, V, B})
    return isnull(state.leaf) || isempty(get(state.leaf))
end


immutable IntervalValueIterator{K, V <: IntervalValue, B}
    t::IntervalBTree{K, V, B}
end


function Base.values{K, V <: IntervalValue, B}(t::IntervalBTree{K, V, B})
    return IntervalValueIterator(t)
end


function Base.start{K, V, B}(it::IntervalValueIterator{K, V, B})
    # traverse to the first leaf node
    node = it.t.root
    while !isa(node, LeafNode{K, V, B})
        node = node.children[1]
    end

    return IntervalBTreeIteratorState(Nullable(node), 1)
end


function Base.next{K, V, B}(t::IntervalValueIterator{K, V, B},
                            state::IntervalBTreeIteratorState{K, V, B})
    leaf = get(state.leaf)
    value = leaf.entries[state.i].value
    if state.i < length(leaf)
        state = IntervalBTreeIteratorState{K, V, B}(leaf, state.i + 1)
    else
        state = IntervalBTreeIteratorState{K, V, B}(leaf.right, 1)
    end
    return value, state
end


function Base.done{K, V, B}(t::IntervalValueIterator{K, V, B},
                            state::IntervalBTreeIteratorState{K, V, B})
    return isnull(state.leaf) || isempty(get(state.leaf))
end
