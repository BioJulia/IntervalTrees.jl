

module IntervalTrees

import Base: start, next, done, haskey, length, isempty, setindex!, delete!

export IntervalTree, depth

# Each of these types is indexes by K, V, B, where
#   K : Interval type. Intervals are represented as (K, K) tuples.
#   V : Value type. May be anything.
#   B : Integer giving the B-tree order.

abstract Node{K, V, B}

immutable NullNode{K, V, B} <: Node{K, V, B} end


function isnull(node::Node)
    return isa(node, NullNode)
end


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
        sizehint(t.maxends, B)
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
    resize!(right.maxends, m - div(m, 2))
    resize!(right.keys, m - div(m, 2) - 1)

    right.children[1:end] = left.children[div(m, 2)+1:end]
    right.maxends[1:end] = left.maxends[div(m, 2)+1:end]
    right.keys[1:end] = left.keys[div(m, 2)+1:end]

    resize!(left.children, div(m, 2))
    resize!(left.maxends, div(m, 2))
    resize!(left.keys, div(m, 2) - 1)

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
        (leftnode, rightnode, median, leftmaxend, rightmaxend) = ans
        # we need a new root
        t.root = InternalNode{K, V, B}()
        push!(t.root.keys, median)
        push!(t.root.maxends, leftmaxend)
        push!(t.root.maxends, rightmaxend)
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
        if key[2] > t.maxends[j]
            t.maxends[j] = key[2]
        end

        return ans
    else
        (leftnode, rightnode, median, leftmaxend, rightmaxend) = ans

        insert!(t.children, j + 1, rightnode)
        insert!(t.maxends, j + 1, rightmaxend)
        t.maxends[j] = leftmaxend
        insert!(t.keys, j, median)

        # split when full
        if length(t) == B
            (leftnode, rightnode) = split!(t)
            return (leftnode, rightnode, nodemin(rightnode),
                    nodemaxend(leftnode), nodemaxend(rightnode))
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
                    nodemaxend(leftleaf), nodemaxend(rightleaf))
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


# Delete key from the subtree is present. Return a pair of Bools (keyfound,
# delete_child, maxend, minstart). Keyfound is true if the key was found,
# delete_child is true if t should be deleted from its parent.
function _delete!{K, V, B}(t::InternalNode{K, V, B}, key::(K, K))
    i = findidx(t, key)
    j = i <= length(t) - 1 && key >= t.keys[i] ? i + 1 : i # child index

    keyfound, delete_child, maxend, minstart = _delete!(t.children[j], key)

    if delete_child
        child = t.children[j]
        splice!(t.children, j)

        if j == 1
            splice!(t.keys, 1)
            splice!(t.maxends, 1)
        else
            splice!(t.keys, j - 1)
            splice!(t.maxends, j - 1)
        end

        # TODO: update keys and maxend

        # TODO: are we underful. Do the whole song and dance from the leaf
        # node over again.

    elseif keyfound
        # This is actually really tricky. Passing the child's maxend is not
        # enough, since a key maxend may come from the other branch.

        # Maybe I should be storing a maxend field for every child, not every
        # key.

        # Fuuuck. We don't actually now wether maxend has changed, since
        # there may be another item with the same maxend. This seems like
        # something I need to pass upwards.

        # TODO: we may need to update keys and maxend
    end

    return (keyfound, false)
end


function _delete!{K, V, B}(t::LeafNode{K, V, B}, key::(K, K))
    i = findidx(t, key)

    # do nothing if the key isn't present
    if i < 1 || i > length(t) || t.keys[i] != key
        return (false, false)
    end

    splice!(t.keys, i)
    splice!(t.values, i)

    minsize = div(B, 2)

    # not underfull
    if length(t) >= minsize
        return (true, false)
    end

    if isempty(t)
        return (true, true)
    end

    # borrow right
    if !isnull(t.right) && t.right.parent == t.parent && length(t.right) > minsize
        push!(t.keys, splice!(t.right.keys, 1))
        push!(t.values, splice!(t.right.values, 1))
        return (true, false)
    end

    # borrow left
    if !isnull(t.left) && t.left.parent == t.parent && length(t.left) > minsize
        insert!(t.keys, 1, pop!(t.left.keys))
        insert!(t.values, 1, pop!(t.left.values))
        return (true, false)
    end

    # merge left
    if !isnull(t.left) && t.left.parent == t.parent
        merge!(t.left, t)
        t.left.right = t.right
        if !isnull(t.right)
            t.right.left = t.left
        end
        return (true, true)
    end

    # merge right
    if !isnull(t.right) && t.right.parent == t.parent
        merge!(t.right, t)
        t.right.left = t.left
        if !isnull(t.left)
            t.left.right = t.right
        end
        return (true, true)
    end

    error("Malformed interval tree. This is a bug.")
end


# Join two leaf nodes into one.
function merge!{K, V, B}(left::LeafNode{K, V, B}, right::LeafNode{K, V, B})
    leftlen, rightlen = length(left), length(rigth)
    @assert leftlen + rightlen <= B
    resize!(right.keys, leftlen + rightlen)
    resize!(right.values, leftlen + rightlen)
    left.keys[leftlen+1:end] = right.keys
    left.values[leftlen+1:end] = right.values
    return
end

# Join two internal nodes into one
function merge!{K, V, B}(left::InternalNode{K, V, B}, right::InternalNode{K, V, B})
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


