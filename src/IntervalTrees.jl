

module IntervalTrees

import Base: start, next, done, haskey, length, isempty, getindex, setindex!,
             get, get!, delete!, push!, pop!, resize!, insert!, splice!, copy!,
             size, searchsortedfirst, isless, intersect

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
        node = node.children[1]
    end

    return (node, 1)
end


function next(t::IntervalBTree, state)
    leaf, i = state
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


# Find the first leaf node in the tree
function firstleaf(t::IntervalBTree)
    return firstleaf(t.root)
end


function firstleaf(t::InternalNode)
    return firstleaf(t.children[1])
end


function firstleaf(t::LeafNode)
    return t
end


function nextleafkey(t::LeafNode, i::Integer)
    if i < length(t)
        return (t, i + 1)
    else
        return (t.right, 1)
    end
end


# Find the minimum interval in a subtree
function minkey(t::InternalNode)
    return minkey(t.children[1])
end

function minkey(t::LeafNode)
    return t.keys[1]
end


# Object returned by _setindex
immutable SetIndexResult{K, V, B}
    # true if a new key/value was inserted
    inserted::Bool

    # value that was inserted or found
    value::V

    # if the child node was split, these fields are non-nothing:

    # result of a node split
    leftnode::Union(Node{K, V, B}, Nothing)
    rightnode::Union(Node{K, V, B}, Nothing)

    # key separating leftnode and rightnode
    median::Union(Interval{K}, Nothing)

    # new maxend
    maxend::Union(K, Nothing)
end


function setindex!{K, V, B}(t::IntervalBTree{K, V, B}, value0, key0::(Any, Any),
                            noupdate::Bool=false)
    key = Interval{K}(key0[1], key0[2])
    value = convert(V, value0)

    result = _setindex!(t.root, value, key, noupdate)
    if result.leftnode === nothing
        if result.inserted
            t.n += 1
        end
    else
        # we need a new root
        t.root = InternalNode{K, V, B}()
        push!(t.root.keys, result.median)
        push!(t.root.children, result.leftnode)
        push!(t.root.children, result.rightnode)
        t.root.maxend = result.maxend
        result.leftnode.parent = t.root
        result.rightnode.parent = t.root
        t.n += 1
    end

    return result.value
end


function _setindex!{K, V, B}(t::InternalNode{K, V, B}, value::V,
                             key::Interval{K}, noupdate::Bool)
    i = findidx(t, key) # key index
    j = i <= length(t) - 1 && key >= t.keys[i] ? i + 1 : i # child index
    result = _setindex!(t.children[j], value, key, noupdate)

    if result.leftnode === nothing
        # update maxend if needed
        if key.b > t.maxend
            t.maxend = key.b
        end

        return result
    else
        insert!(t.children, j + 1, result.rightnode)
        if result.maxend > t.maxend
            t.maxend = result.maxend
        end
        insert!(t.keys, j, result.median)

        # split when full
        if length(t) == B
            leftnode, rightnode = split!(t)
            return SetIndexResult{K, V, B}(true, result.value, leftnode, rightnode,
                                           minkey(rightnode),
                                           max(leftnode.maxend, rightnode.maxend))
        else
            return SetIndexResult{K, V, B}(true, result.value, nothing, nothing,
                                           nothing, nothing)
        end
    end
end


function _setindex!{K, V, B}(t::LeafNode{K, V, B}, value::V, key::Interval{K},
                             noupdate::Bool)
    i = max(1, findidx(t, key))
    if i <= length(t) && t.keys[i] == key
        if noupdate
            return SetIndexResult{K, V, B}(false, t.values[i], nothing, nothing,
                                           nothing, nothing)
        else
            t.values[i] = value
            return SetIndexResult{K, V, B}(false, value, nothing, nothing,
                                           nothing, nothing)
        end
    else
        insert!(t.keys, i, key)
        insert!(t.values, i, value)
        if length(t) == 1 || key.b > t.maxend
            t.maxend = key.b
        end

        # split when full
        if length(t) == B
            leftleaf, rightleaf = split!(t)
            return SetIndexResult{K, V, B}(true, value, leftleaf, rightleaf,
                                           rightleaf.keys[1],
                                           max(leftleaf.maxend, rightleaf.maxend))
        else
            return SetIndexResult{K, V, B}(true, value, nothing, nothing,
                                           nothing, nothing)
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

    return t
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

        # deleteidx == 1 would only happen if we had only one child, but if
        # that were the case it would have been promoted.
        @assert deleteidx > 1

        splice!(t.keys, deleteidx - 1)
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
        t.maxend = max(t.maxend, t.keys[end].b)
        t.right.maxend = nodemaxend(t.right)

        return DeleteResult(true, :updateright)
    end

    # borrow left
    if !isnull(t.left) && t.left.parent == t.parent && length(t.left) > minsize
        insert!(t.keys, 1, pop!(t.left.keys))
        insert!(t.values, 1, pop!(t.left.values))
        t.maxend = max(t.maxend, t.keys[1].b)
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
    for i in leftlen+1:length(left.children)
        left.children[i].parent = left
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
    if isempty(t.keys)
        return 0
    end
    return searchsortedfirst(t.keys, key)
end

function findidx{K, V, B}(t::InternalNode{K, V, B}, key::Interval{K})
    if isempty(t.keys)
        return 0
    end
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


function get!{K, V, B}(t::IntervalBTree{K, V, B}, key0::(Any, Any), default)
    return setindex!(t, default, key0, true)
end


function get{K, V, B}(t::IntervalBTree{K, V, B}, key0::(Any, Any), default)
    key = Interval{K}(key0[1], key0[2])
    return _get(t.root, key, default)
end


function _get{K, V, B}(t::InternalNode{K, V, B}, key::Interval{K}, default)
    i = findidx(t, key)
    if 1 <= length(t) - 1 && key >= t.keys[i]
        return _get(t.children[i+1], key, default)
    else
        return _get(t.children[i], key, default)
    end
end


function _get{K, V, B}(t::LeafNode{K, V, B}, key::Interval{K}, default)
    i = findidx(t, key)
    if 1 <= i <= length(t) && t.keys[i] == key
        return t.values[i]
    else
        return default
    end
end


function getindex{K, V, B}(t::IntervalBTree{K, V, B}, key0::(Any, Any))
    key = Interval{K}(key0[1], key0[2])
    return _getindex(t.root, key)
end


function _getindex{K, V, B}(t::InternalNode{K, V, B}, key::Interval{K})
    i = findidx(t, key)
    if 1 <= length(t) - 1 && key >= t.keys[i]
        return _getindex(t.children[i+1], key)
    else
        return _getindex(t.children[i], key)
    end
end


function _getindex{K, V, B}(t::LeafNode{K, V, B}, key::Interval{K})
    i = findidx(t, key)
    if 1 <= i <= length(t) && t.keys[i] == key
        return t.values[i]
    else
        error(KeyError((key.a, key.b)))
    end
end


# Intersection
# ------------

# There are two ways we can tackle intersection: traversal (search the tree for
# intervals that intersect) and iteration (iterate through the tree's intervals
# in sorted order).
#
# Traversal is roughly O(log(n)) while iteration is roughly O(n). Obviously
# if we are doing a single intersection, we would choose traversal, but when
# intersecting two trees it becomes more interesting.
#
# If the second tree is of size m, then intersection by traversal is
# O(min(m * log(n), n * log(m))) while interesction by iteration is O(m + n).
# When m and n are both large, it becomes more efficient to intersect by
# iteration, but deciding when requires some heuristics.
#

# Return true iff two key1 and key2 intersect.
function intersects{K}(key1::Interval{K}, key2::Interval{K})
    return key1.a <= key2.b && key2.a <= key1.b
end


# Find the first interval in the tree that intersects the query and return
# as a (leafnode, index) pair, indicating that leafnode.keys[index] intersects.
# If no intersection is found, index is 0 and leafnode is the last node
# searched.
function firstintersection{K, V, B}(t::IntervalBTree{K, V, B}, query::Interval{K})
    return firstintersection(t.root, query)
end


function firstintersection{K, V, B}(t::InternalNode{K, V, B}, query::Interval{K})
    if isempty(t) || t.maxend < query.a
        return (t, 0)
    end

    for (i, child) in enumerate(t.children)
        if child.maxend >= query.a && (i == 1 || t.keys[i-1].a <= query.b)
            s, j = firstintersection(child, query)
            if j > 0
                return (s, j)
            end
        elseif child.keys[1].a > query.b
            break
        end
    end

    return (t, 0)
end


function firstintersection{K, V, B}(t::LeafNode{K, V, B}, query::Interval{K})
    if isempty(t) || t.maxend < query.a
        return (t, 0)
    end

    for i in 1:length(t)
        if intersects(t.keys[i], query)
            return (t, i)
        elseif query.b < t.keys[i].a
            break
        end
    end

    return (t, 0)
end


# If query intersects node.keys[i], return the next intersecting key as
# a (leafnode, index) pair.
function nextintersection{K, V, B}(t::LeafNode{K, V, B}, i::Integer,
                                   query::Interval{K})
    u = t
    j = i + 1
    while true
        while j <= length(u)
            if intersects(u.keys[j], query)
                return (u, j)
            end
            j += 1
        end
        j = 1
        u = u.right
        if isa(u, NullNode) || u.keys[1].a > query.b
            break
        end
    end

    return (u, 0)
end


immutable IntervalIntersectionIterator{K, V, B}
    t::IntervalBTree{K, V, B}
    query::Interval{K}
end


# Intersect an interval tree t with a single interval, returning an iterator
# over the intersecting (key, value) pairs in t.
function intersect{K, V, B}(t::IntervalBTree{K, V, B}, query0::(Any, Any))
    query = Interval{K}(query0[1], query0[2])
    return intersect(t, query)
end


function intersect{K, V, B}(t::IntervalBTree{K, V, B}, query::Interval{K})
    return IntervalIntersectionIterator(t, query)
end


function start{K, V, B}(it::IntervalIntersectionIterator{K, V, B})
    return firstintersection(it.t, it.query)
end


function next{K, V, B}(it::IntervalIntersectionIterator{K, V, B}, state)
    node, index = state
    nextitem = ((node.keys[index].a, node.keys[index].b), node.values[index])
    return (nextitem, nextintersection(node, index, it.query))
end


function done(it::IntervalIntersectionIterator, state)
    node, index = state
    return index == 0
end


# Iterate through the intersection of two trees by iterating through both in
# order.
immutable IterativeTreeIntersectionIterator{K, V, B}
    t1::IntervalBTree{K, V, B}
    t2::IntervalBTree{K, V, B}
end


immutable IterativeTreeIntersectionIteratorState{K, V, B}
    u::Node{K, V, B}
    v::Node{K, V, B}
    w::Node{K, V, B}
    i::Int
    j::Int
end


function start{K, V, B}(it::IterativeTreeIntersectionIterator{K, V, B})
    # Notation notes. We proceed by finding all the the intervals
    # in t2 that intersect u.keys[i], before proceeding to the next
    # key in t1.
    #
    # We use v to keep track of the first leaf node in t2 that might contain an
    # intersecting interval, # and with (w, j) the current intersecting entry in
    # t2. We need to hang onto v since we will need to jump back when the
    # entry in t1 gets incremented.
    #
    # The thing to keep in mind is that this is just like the merge operation in
    # mergesort, except that some backtracking is needed since intersection
    # isn't as simple as ordering.
    u, v = firstleaf(it.t1), firstleaf(it.t2)
    w = v
    i, j = 1, 1

    return nextintersection(it,
        IterativeTreeIntersectionIteratorState{K, V, B}(u, v, w, i, j))
end


# This is fantastically slow. I get that these intervals are all over the place.

function nextintersection{K, V, B}(it::IterativeTreeIntersectionIterator{K, V, B},
                                   state::IterativeTreeIntersectionIteratorState{K, V, B})
    u, v, w, i, j = state.u, state.v, state.w, state.i, state.j
    while !isa(u, NullNode) && !isa(v, NullNode)
        if isa(w, NullNode)
            u, i = nextleafkey(u, i)
            w, j = v, 1
        elseif u.maxend < v.keys[1].a
            u = u.right
            i = 1
        elseif v.maxend < u.keys[i].a
            v, w = v.right, v.right
            j = 1
        elseif intersects(u.keys[i], w.keys[j])
            return IterativeTreeIntersectionIteratorState{K, V, B}(u, v, w, i, j)
        elseif u.keys[i] < w.keys[j]
            u, i = nextleafkey(u, i)
            w, j = v, 1
        elseif u.keys[i] >= w.keys[j]
            w, j = nextleafkey(w, j)
        else
            error("Malformed IntervalTree")
        end
    end

    return IterativeTreeIntersectionIteratorState{K, V, B}(u, v, w, i, j)
end


function next{K, V, B}(it::IterativeTreeIntersectionIterator{K, V, B}, state)
    u, v, w, i, j = state.u, state.v, state.w, state.i, state.j
    value = (((u.keys[i].a, u.keys[i].b), u.values[i]),
             ((w.keys[j].a, w.keys[j].b), w.values[j]))
    w, j = nextleafkey(w, j)
    nextstate = nextintersection(it,
        IterativeTreeIntersectionIteratorState{K, V, B}(u, v, w, i, j))
    return (value, nextstate)
end


function done(it::IterativeTreeIntersectionIterator, state)
    return isa(state.u, NullNode) || isa(state.v, NullNode)
end


# Iterate through the intersection of two interval trees by doing single
# interval queries against t2 for every key in t1.
immutable SuccessiveTreeIntersectionIterator{K, V, B}
    t1::IntervalBTree{K, V, B}
    t2::IntervalBTree{K, V, B}
end


function start{K, V, B}(it::SuccessiveTreeIntersectionIterator{K, V, B})
    t1_state = start(it.t1)
    while !done(it.t1, t1_state)
        t1_value, t1_state = next(it.t1, t1_state)
        intersect_it = intersect(it.t2, t1_value[1])
        intersect_state = start(intersect_it)
        if !done(intersect_it, intersect_state)
            return (t1_state, t1_value, intersect_it, intersect_state)
        end
    end

    return (t1_state, nothing, nothing, nothing)
end


function next{K, V, B}(it::SuccessiveTreeIntersectionIterator{K, V, B}, state)
    t1_state, t1_value, intersect_it, intersect_state = state
    intersect_value, intersect_state = next(intersect_it, intersect_state)
    return_value = (t1_value, intersect_value)
    while done(intersect_it, intersect_state) && !done(it.t1, t1_state)
        t1_value, t1_state = next(it.t1, t1_state)
        intersect_it = intersect(it.t2, t1_value[1])
        intersect_state = start(intersect_it)
        intersect_state
    end

    return (return_value, (t1_state, t1_value, intersect_it, intersect_state))
end


function done(it::SuccessiveTreeIntersectionIterator, state)
    t1_state, t1_value, intersect_it, intersect_state = state
    return done(intersect_it, intersect_state) && done(it.t1, t1_state)
end


# Intersect two interval trees, returning an iterator yielding values of the
# form:
#   ((key1, value1), (key2, value2))
#
# Where key1 is from the first tree and key2 from the second, and they
# intersect.
function intersect{K, V, B}(t1::IntervalBTree{K, V, B},
                            t2::IntervalBTree{K, V, B})
    # We decide heuristically which intersection algorithm to use.
    n = length(t1)
    m = length(t2)

    cost1 = n + m
    cost2 = n * log(1 + m)
    cost3 = n * log(1 + n)
    if cost1 <= cost2 <= cost3
        return IterativeTreeIntersectionIterator(t1, t2)
    elseif cost2 <= cost3
        return SuccessiveTreeIntersectionIterator(t1, t2)
    else
        return SuccessiveTreeIntersectionIterator(t2, t1)
    end
end



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

