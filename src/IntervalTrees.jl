

module IntervalTrees

import Base: start, next, done, haskey, length, isempty, getindex, setindex!,
             delete!, push!, pop!, resize!, insert!, splice!, copy!, size,
             searchsortedfirst, isless

export IntervalTree, depth

include("slice.jl")


immutable Interval{T}
    a::T
    b::T
end


function isless(u::Interval, v::Interval)
    return u.a < v.a || (u.a == v.a && u.b < v.b)
end


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
    keys::Slice

    # Maximum interval end-point in this subtree.
    maxend::K

    children::Slice{Node{K, V, B}, B}

    parent::Union(NullNode{K, V, B}, InternalNode{K, V, B})

    # Sibling/cousin pointers.
    left::Union(NullNode{K, V, B}, InternalNode{K, V, B})
    right::Union(NullNode{K, V, B}, InternalNode{K, V, B})

    function InternalNode()
        t = new(Slice{Interval{K}, B - 1}(), zero(K), Slice{Node{K, V, B}, B}(),
                NullNode{K, V, B}(), NullNode{K, V, B}(), NullNode{K, V, B}())
        return t
    end
end


type LeafNode{K, V, B} <: Node{K, V, B}
    # Unlike internal nodes, the keys here coorespond to the actual stored
    # intervals.
    keys::Slice{Interval{K}, B}

    # Value i corresponds to the intervals in keys[i].
    values::Slice{V, B}

    # maximum interval end-point in this leaf node.
    maxend::K

    parent::Union(NullNode{K, V, B}, InternalNode{K, V, B})

    # Sibling/cousin pointers.
    left::Union(NullNode{K, V, B}, LeafNode{K, V, B})
    right::Union(NullNode{K, V, B}, LeafNode{K, V, B})

    function LeafNode()
        t = new(Slice{Interval{K}, B}(),
                Slice{V, B}(),
                zero(K),
                NullNode{K, V, B}(),
                NullNode{K, V, B}(),
                NullNode{K, V, B}())
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
        return (((key.a, key.b), value), (leaf, i + 1))
    else
        return (((key.a, key.b), value), (leaf.right, 1))
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

    copy!(right.keys, 1, left.keys, div(m, 2) + 1, length(right.keys))
    copy!(right.values, 1, left.values, div(m, 2) + 1, length(right.values))

    resize!(left.keys, div(m, 2))
    resize!(left.values, div(m, 2))

    left.maxend = maximum([key.b for key in left.keys])
    right.maxend = maximum([key.b for key in right.keys])

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

    copy!(right.children, 1, left.children, div(m, 2)+1, length(right.children))
    copy!(right.keys, 1, left.keys, div(m, 2)+1, length(right.keys))

    resize!(left.children, div(m, 2))
    resize!(left.keys, div(m, 2) - 1)

    left.maxend = maximum([child.maxend for child in left.children])
    right.maxend = maximum([child.maxend for child in right.children])

    for child in right.children
        child.parent = right
    end

    return (left, right)
end


# Find the maximum interval end point is a subtree
function nodemaxend{K, V, B}(t::InternalNode{K, V, B})
    maxend = zero(K)
    for i in 1:length(t.children)
        if t.children[i].maxend > maxend
            maxend = t.children[i].maxend
        end
    end
    return maxend
end


function nodemaxend{K, V, B}(t::LeafNode{K, V, B})
    maxend = zero(K)
    for i in 1:length(t.keys)
        if t.keys[i].b > maxend
            maxend = t.keys[i].b
        end
    end
    return maxend
end


# Find the minimum interval in a subtree
function minkey(t::InternalNode)
    return minkey(t.children[1])
end

function minkey(t::LeafNode)
    return t.keys[1]
end


function setindex!{K, V, B}(t::IntervalBTree{K, V, B}, value0, key0::(Any, Any))
    key = Interval{K}(key0[1], key0[2])
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
        push!(t.root.children, leftnode)
        push!(t.root.children, rightnode)
        t.root.maxend = maxend
        leftnode.parent = t.root
        rightnode.parent = t.root
        t.n += 1
    end

    return value
end


function _setindex!{K, V, B}(t::InternalNode{K, V, B}, value::V, key::Interval{K})
    i = findidx(t, key) # key index
    j = i <= length(t) - 1 && key >= t.keys[i] ? i + 1 : i # child index
    ans = _setindex!(t.children[j], value, key)

    if isa(ans, Bool)
        # update maxend if needed
        if key.b > t.maxend
            t.maxend = key.b
        end

        return ans
    else
        (leftnode, rightnode, median, maxend) = ans

        insert!(t.children, j + 1, rightnode)
        if maxend > t.maxend
            t.maxend = maxend
        end
        insert!(t.keys, j, median)

        # split when full
        if length(t) == B
            (leftnode, rightnode) = split!(t)
            return (leftnode, rightnode, minkey(rightnode),
                    max(leftnode.maxend, rightnode.maxend))
        else
            return true
        end
    end
end


function _setindex!{K, V, B}(t::LeafNode{K, V, B}, value::V, key::Interval{K})
    i = max(1, findidx(t, key))
    if i <= length(t) && t.keys[i] == key
        t.values[i] = value
        return false
    else
        insert!(t.keys, i, key)
        insert!(t.values, i, value)
        if length(t) == 1 || key.b > t.maxend
            t.maxend = key.b
        end

        # split when full
        if length(t) == B
            (leftleaf, rightleaf) = split!(t)
            return (leftleaf, rightleaf, rightleaf.keys[1],
                    max(leftleaf.maxend, rightleaf.maxend))
        else
            return true
        end
    end
end



# Deleting
# --------


function delete!{K, V, B}(t::IntervalBTree{K, V, B}, key0::(Any, Any))
    key = Interval{K}(key0[1], key0[2])
    ans = _delete!(t.root, key)

    if ans.keyfound
        t.n -= 1
    end

    # if the root has only one child, promote the child
    if isa(t.root, InternalNode) && length(t.root) == 1
        t.root = t.root.children[1]
        t.root.parent = NullNode{K, V, B}()
    end

    return
end


# Information returned from a call to _delete
immutable DeleteResult
    keyfound::Bool # true if the key was found

    # Indicate what steps are need to account for an updated child. One of:
    #   :delete -> Delet the child node.
    #   :deleteright -> Delete the child's right sibling.
    #   :updateleft -> Update the key separating the node from its left sibling
    #   :updateright -> Update the key separating the node from its right #   sibling
    #   :none -> No changes needed.
    fate::Symbol
end


# Delete key from the subtree is present. Return a DeleteResult object.
function _delete!{K, V, B}(t::InternalNode{K, V, B}, key::Interval{K})
    i = findidx(t, key)
    j = i <= length(t) - 1 && key >= t.keys[i] ? i + 1 : i # child index

    ans = _delete!(t.children[j], key)

    if !ans.keyfound
        return DeleteResult(false, :none)
    end

    if ans.fate == :none
        t.maxend = nodemaxend(t)
        return DeleteResult(true, :none)
    elseif ans.fate == :updateleft
        t.maxend = nodemaxend(t)
        if j > 1
            t.keys[j - 1] = minkey(t.children[j]);
            return DeleteResult(true, :none)
        else
            return DeleteResult(true, :updateleft)
        end
    elseif ans.fate == :updateright
        t.maxend = nodemaxend(t)
        if j < length(t)
            t.keys[j] = minkey(t.children[j+1])
        end

        if j > 1
            t.keys[j - 1] = minkey(t.children[j]);
            return DeleteResult(true, :none)
        else
            return DeleteResult(true, :updateleft)
        end
    elseif ans.fate == :delete || ans.fate == :deleteright
        deleteidx = ans.fate == :delete ? j : j + 1
        if deleteidx == 1
            splice!(t.keys, 1)
        else
            splice!(t.keys, deleteidx - 1)
        end
        splice!(t.children, deleteidx)
        t.maxend = nodemaxend(t)

        # not underfull
        minsize = div(B, 2)
        if length(t) >= minsize
            return DeleteResult(true, deleteidx == 1 ? :updateleft : :none)
        end

        # borrow right
        if !isnull(t.right) && t.right.parent == t.parent && length(t.right) > minsize
            splice!(t.right.keys, 1)
            push!(t.children, splice!(t.right.children, 1))
            push!(t.keys, minkey(t.children[end]))
            t.maxend = max(t.maxend, nodemaxend(t.children[end]))
            t.right.maxend = nodemaxend(t.right)
            t.children[end].parent = t

            return DeleteResult(true, :updateright)
        end

        # borrow left
        if !isnull(t.left) && t.left.parent == t.parent && length(t.left) > minsize
            insert!(t.children, 1, pop!(t.left.children))
            pop!(t.left.keys)
            insert!(t.keys, 1, minkey(t.children[2]))
            t.maxend = max(t.maxend, nodemaxend(t.children[1]))
            t.left.maxend = nodemaxend(t.left)
            t.children[1].parent = t

            return DeleteResult(true, :updateleft)
        end

        # merge with left
        if !isnull(t.left) && t.left.parent == t.parent
            merge!(t.left, t)
            t.left.right = t.right
            if !isnull(t.right)
                t.right.left = t.left
            end

            return DeleteResult(true, :delete)
        end

        # merge with right
        if !isnull(t.right) && t.right.parent == t.parent
            merge!(t, t.right)
            if !isnull(t.right.right)
                t.right.right.left = t
            end
            t.right = t.right.right

            return DeleteResult(true, :deleteright)
        end

        # Allow the root node to be underfull
        return DeleteResult(true, :none)
    end

    error("Undefined child fate encountered.")
end


function _delete!{K, V, B}(t::LeafNode{K, V, B}, key::Interval{K})
    i = findidx(t, key)

    # do nothing if the key isn't present
    if i < 1 || i > length(t) || t.keys[i] != key
        return DeleteResult(false, :none)
    end

    splice!(t.keys, i)
    splice!(t.values, i)

    # This is the root node. Allow it to be empty.
    if isempty(t)
        return DeleteResult(true, :none)
    end

    minsize = div(B, 2)

    t.maxend = nodemaxend(t)

    # not underfull
    if length(t) >= minsize
        return DeleteResult(true, i == 1 ? :updateleft : :none)
    end

    if isempty(t)
        return DeleteResult(true, :delete)
    end

    # borrow right
    if !isnull(t.right) && t.right.parent == t.parent && length(t.right) > minsize
        push!(t.keys, splice!(t.right.keys, 1))
        push!(t.values, splice!(t.right.values, 1))
        t.maxend = max(t.maxend, t.keys[end][2])
        t.right.maxend = nodemaxend(t.right)

        return DeleteResult(true, :updateright)
    end

    # borrow left
    if !isnull(t.left) && t.left.parent == t.parent && length(t.left) > minsize
        insert!(t.keys, 1, pop!(t.left.keys))
        insert!(t.values, 1, pop!(t.left.values))
        t.maxend = max(t.maxend, t.keys[1][2])
        t.left.maxend = nodemaxend(t.left)

        return DeleteResult(true, :updateleft)
    end

    # merge with left
    if !isnull(t.left) && t.left.parent == t.parent
        merge!(t.left, t)
        t.left.right = t.right
        if !isnull(t.right)
            t.right.left = t.left
        end
        return DeleteResult(true, :delete)
    end

    # merge with right
    if !isnull(t.right) && t.right.parent == t.parent
        merge!(t, t.right)
        if !isnull(t.right.right)
            t.right.right.left = t
        end
        t.right = t.right.right
        return DeleteResult(true, :deleteright)
    end

    # This must be the root node. Allow it to be underfull.
    return DeleteResult(true, :none)
end


# Join two leaf nodes into one.
function merge!{K, V, B}(left::LeafNode{K, V, B}, right::LeafNode{K, V, B})
    leftlen, rightlen = length(left), length(right)
    @assert leftlen + rightlen <= B
    resize!(left.keys, leftlen + rightlen)
    resize!(left.values, leftlen + rightlen)
    copy!(left.keys, leftlen+1, right.keys, 1, length(right.keys))
    copy!(left.values, leftlen+1, right.values, 1, length(right.values))
    left.maxend = nodemaxend(left)
    return
end


# Join two internal nodes into one
function merge!{K, V, B}(left::InternalNode{K, V, B}, right::InternalNode{K, V, B})
    leftlen, rightlen = length(left), length(right)
    @assert length(left) + length(right) <= B
    resize!(left.keys, leftlen + rightlen - 1)
    resize!(left.children, leftlen + rightlen)
    copy!(left.children, leftlen+1, right.children, 1, length(right.children))
    for child in left.children[leftlen+1:end]
        child.parent = left
    end
    left.keys[leftlen] = minkey(left.children[leftlen+1])
    copy!(left.keys, leftlen+1, right.keys, 1, length(right.keys))
    left.maxend = nodemaxend(left)
    return
end


# Searching
# ---------

# Find index where a key belongs in internal and leaf nodes.
function findidx{K, V, B}(t::LeafNode{K, V, B}, key::Interval{K})
    return searchsortedfirst(t.keys, key)
end

function findidx{K, V, B}(t::InternalNode{K, V, B}, key::Interval{K})
    i = searchsortedfirst(t.keys, key)
    return min(i, length(t.keys))
end

function haskey(t::NullNode, key)
    return false
end

function haskey{K, V, B}(t::LeafNode{K, V, B}, key::Interval{K})
    i = findidx(t, key)
    return 1 <= i <= length(t) && t.keys[i] == key
end


function haskey{K, V, B}(t::InternalNode{K, V, B}, key::Interval{K})
    i = findidx(t, key)
    if i <= length(t) - 1 && key >= t.keys[i]
        return haskey(t.children[i+1], key)
    else
        return haskey(t.children[i], key)
    end
end


function haskey{K, V, B}(t::IntervalBTree{K, V, B}, key0::(Any, Any))
    key = Interval{K}(key0[1], key0[2])
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

