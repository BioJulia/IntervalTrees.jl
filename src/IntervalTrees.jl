

module IntervalTrees

import Base: start, next, done, haskey, length, isempty, setindex!

export IntervalTree

# Each of these types is indexes by K, V, B, where
#   K : Interval type. Intervals are represented as (K, K) tuples.
#   V : Value type. May be anything.
#   B : Integer giving the B-tree order.

abstract Node{K, V, B}

immutable NullNode{K, V, B} <: Node{K, V, B} end

type InternalNode{K, V, B} <: Node{K, V, B}
    # A key takes the form (l, u), where `l` divides the low-end intervals
    # values in the left and right subtree (i.e., all intervals in the left
    # subtree have low-end values < l, and all >= l in the right sub-tree), and
    # u gives the maximum high-end value in either sub-tree.
    keys::Vector{(K, K)}

    children::Vector{Node{K, V, B}}

    function InternalNode()
        t = new(Array((K, K), 0), Array(Node{K, V, B}, 0))
        sizehint(t.keys, B - 1)
        sizehint(t.children, B)
        return t
    end
end


type LeafNode{K, V, B} <: Node{K, V, B}
    # Unlike internal nodes, the keys here coorespond to the actual stored
    # intervals.
    keys::Vector{(K, K)}

    # Value i corresponds to the intervals in keys[i].
    values::Vector{V}

    # Next largest leaf node, for fast iteration.
    sibling::Union(NullNode{K, V, B}, LeafNode{K, V, B})

    function LeafNode()
        t = new(Array((K, K), 0),
                Array(V, 0),
                NullNode{K, V, B}())
        sizehint(t.keys, B)
        sizehint(t.values, B)
        return t
    end
end


type IntervalBTree{K, V, B} <: Associative{(K, K), V}
    root::Node
    n::Int # Number of entries

    function IntervalBTree()
        return new(LeafNode{K, V, B}(), 0)
    end
end


# Default B-tree order
typealias IntervalTree{K, V} IntervalBTree{K, V, 32}


# Length
# ------

function length(t::IntervalBTree)
    return t.n
end


function length(t::InternalNode)
    return length(t.children)
end


function length(t::LeafNode)
    return length(t.keys)
end


function isempty(t::IntervalBTree)
    return t.n == 0
end


function isempty(t::InternalNode)
    return isempty(t.children)
end


function isempty(t::LeafNode)
    return isempty(t.keys)
end



# Iterating
# ---------

function start{K, V, B}(t::IntervalBTree{K, V, B})
    # traverse to the first leaf node
    node = t.root
    while !isa(node, LeafNode)
        if !isempty(node)
            node = node.children[1]
        else
            return (NullNode{K, V, B}(), 1)
        end
    end

    return (node, 1)
end


function next(t::IntervalBTree, state)
    (leaf, i) = state
    key, value = leaf.keys[i], leaf.values[i]

    if i < length(leaf)
        return ((key, value), (leaf, i + 1))
    else
        return ((key, value), (leaf.sibling, 1))
    end
end


function done(t::IntervalBTree, state)
    (leaf, i) = state
    return isa(leaf, NullNode) || (isa(leaf, LeafNode) && isempty(leaf))
end


# Inserting
# ---------


# Split a leaf into two, returning (leftnode, rightnode)
function split!{K, V, B}(left::LeafNode{K, V, B})
    right = LeafNode{K, V, B}()
    right.sibling = left.sibling
    left.sibling = right

    m = length(left)

    resize!(right.keys, m - div(m, 2))
    resize!(right.values, m - div(m, 2))
    right.keys[1:end] = left.keys[div(m, 2)+1:end]
    right.values[1:end] = left.values[div(m, 2)+1:end]

    resize!(left.keys, div(m, 2))
    resize!(left.values, div(m, 2))

    return (left, right)
end


# Split an internal node in two, returning (leftnode, rightnode)
function split!{K, V, B}(left::InternalNode{K, V, B})
    right = InternalNode{K, V, B}()

    m = length(left)

    resize!(right.children, m - div(m, 2))
    resize!(right.keys, m - div(m, 2) - 1)

    right.children[1:end] = left.children[div(m, 2)+1:end]
    right.keys[1:end] = left.keys[div(m, 2)+1:end]

    resize!(left.children, div(m, 2))
    resize!(left.keys, div(m, 2) - 1)

    return (left, right)
end


function nodemaxend(t::InternalNode)
    return maximum(map(x -> x[2], t.keys))
end


function nodemaxend(t::LeafNode)
    return maximum(map(x -> x[2], t.keys))
end


function setindex!{K, V, B}(t::IntervalBTree{K, V, B}, value0, key0::(Any, Any))
    key = convert((K, K), key0)
    if !isequal(key0, key)
        error(key0, " is not a valid key for type ", (K, K))
    end

    value = convert(V, value0)

    ans = _setindex!(t.root, value, key)
    if isa(ans, Bool)
        if ans
            t.n += 1
        end
    else
        (leftnode, rightnode, median, maxend) = ans
        # we need a new root
        t.root = InternalNode{K, V, B}()
        push!(t.root.keys, (median, maxend))
        push!(t.root.children, leftnode)
        push!(t.root.children, rightnode)
        t.n += 1
    end

    return value
end


function _setindex!{K, V, B}(t::InternalNode{K, V, B}, value::V, key::(K, K))
    i = findidx(t, key) # key index
    j = i <= length(t) - 1 && key[1] >= t.keys[i][1] ? i + 1 : i # child index
    ans = _setindex!(t.children[j], value, key)

    if isa(ans, Bool)
        # update maxend if needed
        if j > 1 && key[2] > t.keys[j - 1][2]
            t.keys[j - 1] = (t.keys[j - 1][1], key[2])
        end

        if j <= length(t.keys) && key[2] > t.keys[j][2]
            t.keys[j] = (t.keys[j][1], key[2])
        end

        return ans
    else
        (leftnode, rightnode, median, maxend) = ans

        insert!(t.children, j + 1, rightnode)
        insert!(t.keys, j, (median, maxend))

        if j > 1 && t.keys[j - 1][2] < maxend
            t.keys[j - 1] = (t.keys[j - 1][1], maxend)
        end

        if j < length(t.keys) && t.keys[j + 1][2] < maxend
            t.keys[j + 1] = (t.keys[j + 1][1], maxend)
        end

        # split when full
        if length(t) == B
            (leftnode, rightnode) = split!(t)
            return (leftnode, rightnode, rightnode.keys[1][1],
                    max(nodemaxend(leftnode), nodemaxend(rightnode)))
        else
            return true
        end
    end
end


function _setindex!{K, V, B}(t::LeafNode{K, V, B}, value::V, key::(K, K))
    i = max(1, findidx(t, key))
    if i <= length(t) && t.keys[i] == key
        t.values[i] = value
        return false
    else
        insert!(t.keys, i, key)
        insert!(t.values, i, value)

        # split when full
        if length(t) == B
            (leftleaf, rightleaf) = split!(t)
            return (leftleaf, rightleaf, rightleaf.keys[1][1],
                    max(nodemaxend(leftleaf), nodemaxend(rightleaf)))
        else
            return true
        end
    end
end


# TODO


# Deleting
# --------

# Join two leaf nodes into one.
function join!{K, V, B}(left::LeafNode{K, V, B}, right::LeafNode{K, V, B})
    @assert length(left) + length(right) <= B
    # TODO: rewrite this with resizing
    left.sibling = right.sibling
    left.keys[left.m+1:left.m+right.m] = right.keys[1:right.m]
    left.values[left.m+1:left.m+right.m] = right.values[1:right.m]
    return left
end

# Join two internal nodes into one
function join!{K, V, B}(left::InternalNode{K, V, B}, right::InternalNode{K, V, B})
    @assert length(left) + length(right) <= B
    # TODO
end


# TODO

# Searching
# ---------

# Find index where a key belongs in internal and leaf nodes.
function findidx{K, V, B}(t::LeafNode{K, V, B}, key::(K, K))
    return searchsortedfirst(t.keys, key, 1, length(t), Base.Sort.Forward)
end

function findidx{K, V, B}(t::InternalNode{K, V, B}, key::(K, K))
    i = searchsortedfirst(t.keys, key[1], 1, length(t) - 1,
                          Base.Order.ord(isless, first, false))
    return min(i, length(t) - 1)
end





# Can I write one recursive find function and use callbacks to do insertion?


function haskey(t::NullNode, key)
    return false
end

function haskey{K, V, B}(t::LeafNode{K, V, B}, key::(K, K))
    i = findidx(t, key)
    return 1 <= i <= length(t) && t.keys[i] == key
end


function haskey{K, V, B}(t::InternalNode{K, V, B}, key::(K, K))
    i = findidx(t, key)
    if i <= length(t) - 1 && key[1] >= t.key[i][1]
        return haskey(t.children[i+1], key)
    else
        return haskey(t.children[i], key)
    end
end


function haskey{K, V, B}(t::IntervalBTree{K, V, B}, key0::(Any, Any))
    key = convert((K, K), key0)
    if !isequal(key0, key)
        error(key0, " is not a valid key for type ", (K, K))
    end

    return haskey(t.root, key)
end



# TODO: point intersection


# TODO: interval intersection


end


