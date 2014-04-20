

module IntervalTrees

import Base: start, next, done, haskey, length, isempty, setindex!, delete!

export IntervalTree, depth

# Each of these types is indexes by K, V, B, where
#   K : Interval type. Intervals are represented as (K, K) tuples.
#   V : Value type. May be anything.
#   B : Integer giving the B-tree order.

abstract Node{K, V, B}

immutable NullNode{K, V, B} <: Node{K, V, B} end

type InternalNode{K, V, B} <: Node{K, V, B}
    # Internal nodes are keyed by the minimum interval in the right subtree.  We
    # need internal node keys to be intervals themselves, since ordering by
    # start value alone only works if start values are unique. We don't
    # force that restriction, so we must break ties in order to split nodes that
    # are full of intervals with the same start value.
    keys::Vector{(K, K)}

    # The "interval tree" augmentation. We keey track of the maximum interval
    # end in the keys two subtrees to make intersection tests efficient.
    maxends::Vector{K}

    children::Vector{Node{K, V, B}}

    parent::Union(NullNode{K, V, B}, InternalNode{K, V, B})

    # Sibling/cousin pointers.
    left::Union(NullNode{K, V, B}, InternalNode{K, V, B})
    right::Union(NullNode{K, V, B}, InternalNode{K, V, B})

    function InternalNode()
        t = new(Array((K, K), 0), Array(K, 0), Array(Node{K, V, B}, 0),
                NullNode{K, V, B}(), NullNode{K, V, B}(), NullNode{K, V, B}())
        sizehint(t.keys, B - 1)
        sizehint(t.maxends, B - 1)
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

    parent::Union(NullNode{K, V, B}, InternalNode{K, V, B})

    # Sibling/cousin pointers.
    left::Union(NullNode{K, V, B}, LeafNode{K, V, B})
    right::Union(NullNode{K, V, B}, LeafNode{K, V, B})

    function LeafNode()
        t = new(Array((K, K), 0),
                Array(V, 0),
                NullNode{K, V, B}(),
                NullNode{K, V, B}(),
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
typealias IntervalTree{K, V} IntervalBTree{K, V, 64}


# Length
# ------

function length(t::IntervalBTree)
    return t.n
end


function depth(t::IntervalBTree)
    k = 1
    node = t.root
    while !isa(node, LeafNode)
        k += 1
        node = node.children[1]
    end
    return k
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
        return ((key, value), (leaf.right, 1))
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
    right.right = left.right
    right.left = left
    left.right = right
    if !isa(right.right, NullNode)
        right.right.left = right
    end
    right.parent = left.parent

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
    right.right = left.right
    right.left = left
    left.right = right
    if !isa(right.right, NullNode)
        right.right.left = right
    end
    right.parent = left.parent

    m = length(left)

    resize!(right.children, m - div(m, 2))
    resize!(right.keys, m - div(m, 2) - 1)
    resize!(right.maxends, m - div(m, 2) - 1)

    right.children[1:end] = left.children[div(m, 2)+1:end]
    right.keys[1:end] = left.keys[div(m, 2)+1:end]
    right.maxends[1:end] = left.maxends[div(m, 2)+1:end]

    resize!(left.children, div(m, 2))
    resize!(left.keys, div(m, 2) - 1)
    resize!(left.maxends, div(m, 2) - 1)

    for child in right.children
        child.parent = right
    end

    return (left, right)
end


# Find the maximum interval end point is a subtree
function nodemaxend(t::InternalNode)
    return maximum(t.maxends)
end

function nodemaxend(t::LeafNode)
    return maximum(map(x -> x[2], t.keys))
end


# Find the minimum interval in a subtree
function nodemin(t::InternalNode)
    return nodemin(t.children[1])
end

function nodemin(t::LeafNode)
    return t.keys[1]
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
        push!(t.root.keys, median)
        push!(t.root.maxends, maxend)
        push!(t.root.children, leftnode)
        push!(t.root.children, rightnode)
        leftnode.parent = t.root
        rightnode.parent = t.root
        t.n += 1
    end

    return value
end


function _setindex!{K, V, B}(t::InternalNode{K, V, B}, value::V, key::(K, K))
    i = findidx(t, key) # key index
    j = i <= length(t) - 1 && key >= t.keys[i] ? i + 1 : i # child index
    ans = _setindex!(t.children[j], value, key)

    if isa(ans, Bool)
        # update maxend if needed
        if j > 1 && key[2] > t.maxends[j - 1]
            t.maxends[j - 1] = key[2]
        end

        if j <= length(t.keys) && key[2] > t.maxends[j]
            t.maxends[j] = key[2]
        end

        return ans
    else
        (leftnode, rightnode, median, maxend) = ans

        insert!(t.children, j + 1, rightnode)
        insert!(t.keys, j, median)
        insert!(t.maxends, j, maxend)

        if j > 1 && t.maxends[j - 1] < maxend
            t.maxends[j - 1] = maxend
        end

        if j < length(t.keys) && t.maxends[j + 1] < maxend
            t.maxends[j + 1] = maxend
        end

        # split when full
        if length(t) == B
            (leftnode, rightnode) = split!(t)
            return (leftnode, rightnode, nodemin(rightnode),
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
            return (leftleaf, rightleaf, rightleaf.keys[1],
                    max(nodemaxend(leftleaf), nodemaxend(rightleaf)))
        else
            return true
        end
    end
end



# Deleting
# --------


function delete!{K, V, B}(t::IntervalBTree{K, V, B}, key0::(Any, Any))
    key = convert((K, K), key0)

    # What does this need to return?
    #   1. wether or not the subtree is empty and should be deleted
    _delete!(t.root, key)

    # TODO: Does the root have only one child? Delete it and promote
    # the one child to root.
end


function _delete!{K, V, B}(t::InternalNode{K, V, B}, key::(K, K))
     # TODO
end


function _delete!{K, V, B}(t::LeafNode{K, V, B}, key::(K, K))
    i = findidx(t, key)

    # do nothing if the key isn't present
    if i < 1 || i > length(t) || t.keys[i] != key
        return
    end

    splice!(t.keys, i)
    splice!(t.values, i)

    if length(t) >= div(B, 2)
        return
    end

    # otherwise we are underfull

    # TODO: I'm realizing now that I really need a left and right sibling
    # pointer. Otherwise, if the rightmost leaf gets underful, it has
    # no one to borrow from or merge with.

    # try to borrow from our sibling
    if !isa(t.sibling, NullNode{K, V, B}) && length(t.sibling) > div(B, 2)

    else
        # merge with sibling
    end
end




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


# Searching
# ---------

# Find index where a key belongs in internal and leaf nodes.
function findidx{K, V, B}(t::LeafNode{K, V, B}, key::(K, K))
    return searchsortedfirst(t.keys, key)
end

function findidx{K, V, B}(t::InternalNode{K, V, B}, key::(K, K))
    i = searchsortedfirst(t.keys, key)
    return min(i, length(t.keys))
end

function haskey(t::NullNode, key)
    return false
end

function haskey{K, V, B}(t::LeafNode{K, V, B}, key::(K, K))
    i = findidx(t, key)
    return 1 <= i <= length(t) && t.keys[i] == key
end


function haskey{K, V, B}(t::InternalNode{K, V, B}, key::(K, K))
    i = findidx(t, key)
    if i <= length(t) - 1 && key >= t.keys[i]
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


# TODO: get


# TODO: getindex


# TODO: point intersection


# TODO: interval intersection


# Diagnostics
# -----------


# Dumb tree printing, useful only for debuging.
function showtree(io::IO, t::IntervalBTree)
    showtree(io, t.root, 0)
end


function showtree(t::IntervalBTree)
    showtree(STDOUT, t)
end


function showtree(io::IO, t::InternalNode, indent::Int)
    for (i, child) in enumerate(t.children)
        showtree(io, child, indent+1)
        if i <= length(t.keys)
            print(io, repeat("  ", indent), t.keys[i], ":\n")
        end
    end
end


function showtree(io::IO, t::LeafNode, indent::Int)
    for (k, v) in zip(t.keys, t.values)
        print(io, repeat("  ", indent), k, ": ", v, "\n")
    end
end




end


