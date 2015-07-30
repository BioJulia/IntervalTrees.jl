

module IntervalTrees

using Compat, Docile
using Docile: @doc

import Base: first, last

export IntervalTree, IntervalMap, AbstractInterval, Interval, IntervalValue, depth,
       hasintersection, from, first, last, print, show, value

include("slice.jl")


@doc """
An AbstractInterval{T} must have a `first` and `last` function each returning
a value of type T, and `first(i) <= last(i)` must always be true.
""" ->
abstract AbstractInterval{T}


@doc """
A basic interval.
""" ->
immutable Interval{T} <: AbstractInterval{T}
    first::T
    last::T
end

first{T}(i::Interval{T}) = i.first
last{T}(i::Interval{T}) = i.last

function Base.isless{I <: AbstractInterval, J <: AbstractInterval}(u::I, v::J)
    return first(u) < first(v) || (first(u) == first(v) && last(u) < last(v))
end


@doc """
An interval with some associated data.
""" ->
immutable IntervalValue{K, V} <: AbstractInterval{K}
    first::K
    last::K
    value::V
end

first{K, V}(i::IntervalValue{K, V}) = i.first
last{K, V}(i::IntervalValue{K, V}) = i.last
value{K, V}(i::IntervalValue{K, V}) = i.value

Base.print(io::IO, x::Interval) = print(io, "\n($(first(x)),$(last(x)))")
function Base.show(io::IO, x::Interval)
    t = typeof(x)::DataType
    show(io, t)
    print(io, x)
end

Base.print(io::IO, x::IntervalValue) = print(io, "\n($(first(x)),$(last(x))) => $(value(x))")
function Base.show(io::IO, x::IntervalValue)
    t = typeof(x)::DataType
    show(io, t)
    print(io, x)
end

# Each of these types is indexes by K, V, B, where
#   K : Interval type. Intervals are represented as (K, K) tuples.
#   V : Interval value type. This an interval type that may have associated
#        data. It must have `first`, `last`, and `isless` methods.
#   B : Integer giving the B-tree order.

abstract Node{K, V, B}


type InternalNode{K, V, B} <: Node{K, V, B}
    # Internal nodes are keyed by the minimum interval in the right subtree.  We
    # need internal node keys to be intervals themselves, since ordering by
    # start value alone only works if start values are unique. We don't
    # force that restriction, so we must break ties in order to split nodes that
    # are full of intervals with the same start value.
    keys::Slice{Interval{K}, B}

    # Maximum interval end-point in this subtree.
    maxend::K

    children::Slice{Node{K, V, B}, B}

    parent::Nullable{InternalNode{K, V, B}}

    # Sibling/cousin pointers.
    left::Nullable{InternalNode{K, V, B}}
    right::Nullable{InternalNode{K, V, B}}

    function InternalNode()
        t = new(Slice{Interval{K}, B}(), zero(K), Slice{Node{K, V, B}, B}(),
                Nullable{InternalNode{K, V, B}}(),
                Nullable{InternalNode{K, V, B}}(),
                Nullable{InternalNode{K, V, B}}())
        return t
    end
end


function minstart(t::InternalNode)
    return t.keys[1].first
end


type LeafNode{K, V, B} <: Node{K, V, B}
    entries::Slice{V, B}

    # maximum interval end-point in this leaf node.
    maxend::K

    parent::Nullable{InternalNode{K, V, B}}

    # Sibling/cousin pointers.
    left::Nullable{LeafNode{K, V, B}}
    right::Nullable{LeafNode{K, V, B}}

    function LeafNode()
        t = new(Slice{V, B}(),
                zero(K),
                Nullable{InternalNode{K, V, B}}(),
                Nullable{LeafNode{K, V, B}}(),
                Nullable{LeafNode{K, V, B}}())
        return t
    end
end


function minstart(t::LeafNode)
    return first(t.entries[1])
end



type IntervalBTree{K, V, B}
    root::Node{K, V, B}
    n::Int # Number of entries

    function IntervalBTree()
        return new(LeafNode{K, V, B}(), 0)
    end

    # Construct an interval tree from a sorted array of intervals.
    #
    # This is generally much more efficient than constructing the same tree by
    # inserting intervals one by one.
    #
    # Args:
    #   entries: Interval entry values in sorted order.
    #
    function IntervalBTree(entries::AbstractVector{V})
        if !issorted(entries)
            error("Intervals must be sorted to construct an IntervalTree")
        end

        # Here's the plan: figure out how many leaf nodes we need, allocate
        # them all up front. Copy in the keys and values, then work up towards
        # the root.

        n = length(entries)

        if n == 0
            return new(LeafNode{K, V, B}(), 0)
        end

        d, r =  divrem(n, B - 2)
        numleaves = d + (r > 0 ? 1 : 0)
        leaves = [LeafNode{K, V, B}() for _ in 1:numleaves]

        maxends = Array(K, numleaves)
        minkeys = Array(Interval{K}, numleaves)

        # divy up the keys and values among the leaves
        d, r = divrem(n, numleaves)
        keys_per_leaf = d + (r > 0 ? 1 : 0)
        for i in 1:numleaves
            u = (i - 1) * keys_per_leaf + 1
            v = min(n, i * keys_per_leaf)
            minkeys[i] = Interval{K}(first(entries[u]), last(entries[u]))
            maxends[i] = last(entries[u])
            for j in u:v
                push!(leaves[i].entries, entries[j])
                maxends[i] = max(maxends[i], last(entries[j]))
            end
            leaves[i].maxend = maxends[i]
        end

        children = leaves
        while length(children) > 1
            # sibling pointers
            for i in 1:length(children)-1
                children[i].right = children[i+1]
                children[i+1].left = children[i]
            end

            # make parents
            d, r = divrem(length(children), B - 2)
            numparents = d + (r > 0 ? 1 : 0)
            parents = [InternalNode{K, V, B}() for _ in 1:numparents]

            # divy up children among parents
            d, r = divrem(length(children), numparents)
            children_per_parent = d + (r > 0 ? 1 : 0)
            for i in 1:numparents
                u = (i - 1) * keys_per_leaf + 1
                v = min(length(children), i * keys_per_leaf)
                maxend = maxends[u]
                for j in u:v
                    push!(parents[i].children, children[j])
                    children[j].parent = parents[i]
                    maxend = max(maxend, maxends[j])
                    if j > u
                        push!(parents[i].keys, minkeys[j])
                    end
                end
                minkeys[i] = minkeys[u]
                parents[i].maxend = maxend
                maxends[i] = maxend
            end

            children = parents
        end
        @assert length(children) == 1

        return new(children[1], n)
    end
end


# Default B-tree order
typealias IntervalTree{K, V} IntervalBTree{K, V, 64}

# Show

function Base.show(io::IO, it::IntervalTree)
    t = typeof(it)::DataType
    show(io, t)
    n = length(it)
    if length(it) > 6
        # Hacky random access ...
        for (i, x) in enumerate(it)
            i < 4 && print(x)
        end
        print("\n\u22EE") # Vertical ellipsis
        for (i, x) in enumerate(it)
            i > (n-3) && print(x)
        end
    else
        for x in it
            print(x)
        end
    end
end



# Length
# ------

function Base.length(t::IntervalBTree)
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


function Base.length(t::InternalNode)
    return length(t.children)
end


function Base.length(t::LeafNode)
    return length(t.entries)
end


function Base.isempty(t::IntervalBTree)
    return t.n == 0
end


function Base.isempty(t::InternalNode)
    return isempty(t.children)
end


function Base.isempty(t::LeafNode)
    return isempty(t.entries)
end


# Iterating
# ---------


immutable IntervalBTreeIteratorState{K, V, B}
    leaf::Nullable{LeafNode{K, V, B}}
    i::Int
end


function Base.start{K, V, B}(t::IntervalBTree{K, V, B})
    # traverse to the first leaf node
    node = t.root
    while !isa(node, LeafNode{K, V, B})
        node = node.children[1]
    end

    return IntervalBTreeIteratorState(Nullable(node), 1)
end


function Base.next{K, V, B}(t::IntervalBTree{K, V, B},
                            state::IntervalBTreeIteratorState{K, V, B})
    leaf = get(state.leaf)
    entry = leaf.entries[state.i]
    if state.i < length(leaf)
        state = IntervalBTreeIteratorState{K, V, B}(leaf, state.i + 1)
    else
        state = IntervalBTreeIteratorState{K, V, B}(leaf.right, 1)
    end
    return entry, state
end


function Base.done{K, V, B}(t::IntervalBTree{K, V, B},
                            state::IntervalBTreeIteratorState{K, V, B})
    return isnull(state.leaf) || isempty(get(state.leaf))
end


# Iterate from a given starting from the first interval that intersects a given
# point.

immutable IntervalFromIterator{K, V, B}
    t::IntervalBTree{K, V, B}
    p::K
end

function from{K, V, B}(t::IntervalBTree{K, V, B}, p)
    return IntervalFromIterator{K, V, B}(t, convert(K, p))
end


function Base.start{K, V, B}(it::IntervalFromIterator{K, V, B})
    node, i = firstfrom(it.t, it.p)

    if i == 0
        return IntervalBTreeIteratorState{K, V, B}(Nullable{LeafNode{K, V, B}}(), i)
    else
        return IntervalBTreeIteratorState{K, V, B}(node, i)
    end
end


function Base.next{K, V, B}(it::IntervalFromIterator{K, V, B},
                            state::IntervalBTreeIteratorState{K, V, B})
    return next(it.t, state)
end


function Base.done{K, V, B}(it::IntervalFromIterator{K, V, B},
                            state::IntervalBTreeIteratorState{K, V, B})
    return done(it.t, state)
end


# Inserting
# ---------


# Split a leaf into two, returning (leftnode, rightnode)
function split!{K, V, B}(left::LeafNode{K, V, B})
    right = LeafNode{K, V, B}()
    right.right = left.right
    right.left = left
    left.right = right
    if !isnull(right.right)
        get(right.right).left = right
    end
    right.parent = left.parent

    m = length(left)
    resize!(right.entries, m - div(m, 2))
    copy!(right.entries, 1, left.entries, div(m, 2) + 1, length(right.entries))
    resize!(left.entries, div(m, 2))

    left.maxend = last(left.entries[1])
    for entry in left.entries
        left.maxend = max(left.maxend, last(entry))
    end

    right.maxend = last(right.entries[1])
    for entry in right.entries
        right.maxend = max(right.maxend, last(entry))
    end

    return left, right
end


# Split an internal node in two, returning (leftnode, rightnode)
function split!{K, V, B}(left::InternalNode{K, V, B})
    right = InternalNode{K, V, B}()
    right.right = left.right
    right.left = left
    left.right = right
    if !isnull(right.right)
        get(right.right).left = right
    end
    right.parent = left.parent

    m = length(left)

    resize!(right.children, m - div(m, 2))
    resize!(right.keys, m - div(m, 2) - 1)

    copy!(right.children, 1, left.children, div(m, 2)+1, length(right.children))
    copy!(right.keys, 1, left.keys, div(m, 2)+1, length(right.keys))

    resize!(left.children, div(m, 2))
    resize!(left.keys, div(m, 2) - 1)

    left.maxend = left.children[1].maxend
    for child in left.children
        left.maxend = max(left.maxend, child.maxend)
    end

    right.maxend = right.children[1].maxend
    for child in right.children
        right.maxend = max(right.maxend, child.maxend)
    end

    for child in right.children
        child.parent = right
    end

    return left, right
end


# Find the maximum interval end point is a subtree
function nodemaxend{K, V, B}(t::InternalNode{K, V, B})
    maxend = zero(K)
    for i in 1:length(t.children)
        maxend = max(maxend, t.children[i].maxend)
    end
    return maxend
end


function nodemaxend{K, V, B}(t::LeafNode{K, V, B})
    maxend = zero(K)
    for i in 1:length(t.entries)
        maxend = max(maxend, last(t.entries[i]))
    end
    return maxend
end


function sameparent{K, V, B}(u::Node{K, V, B}, v::Node{K, V, B})
    return (isnull(u.parent) && isnull(v.parent)) ||
           (!isnull(u.parent) && !isnull(v.parent) && get(u.parent) == get(v.parent))
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
        return Nullable(t), i + 1
    else
        return t.right, 1
    end
end


# Find the minimum interval in a subtree
function minkey(t::InternalNode)
    return minkey(t.children[1])
end

function minkey{K, V, B}(t::LeafNode{K, V, B})
    return Interval{K}(first(t.entries[1]), last(t.entries[1]))
end


function Base.push!{K, V, B}(t::IntervalBTree{K, V, B}, entry::V,
                             unique_key::Bool=false, update::Bool=true)
    return _push!(t, t.root, entry, unique_key, update)
end


function _push!{K, V, B}(t::IntervalBTree{K, V, B},
                         node::InternalNode{K, V, B},
                         entry::V, unique_key::Bool, update::Bool)
    i = findidx(node, entry) # key index
    j = i <= length(node) - 1 && entry >= node.keys[i] ? i + 1 : i # child index
    return _push!(t, node.children[j], entry, unique_key, update)
end


function _push!{K, V, B}(t::IntervalBTree{K, V, B},
                         node::LeafNode{K, V, B}, entry::V,
                         unique_key::Bool, update::Bool)
    i = max(1, findidx(node, entry))

    # key exists and we are replacing it
    if i <= length(node) && unique_key &&
        first(node.entries[i]) == first(entry) && last(node.entries[i]) == last(entry)
        if update
            node.entries[i] = entry
            return entry
        else
            return node.entries[i]
        end
    end

    #unsafe_insert!(node.entries, i, entry)
    insert!(node.entries, i, entry)
    if length(node) == 1 || last(entry) > node.maxend
        node.maxend = last(entry)
    end
    t.n += 1

    # split when full
    if length(node) == B
        maxend = node.maxend
        leftleaf, rightleaf = split!(node)
        median = Interval{K}(first(rightleaf.entries[1]),
                             last(rightleaf.entries[1]))

        # travel back up the tree setting maxend values and splitting as needed
        parent = node.parent
        leftnode = leftleaf
        rightnode = rightleaf
        hassplit = true
        while !isnull(parent)
            p = get(parent)
            if hassplit
                i = findidx(p, entry) # key index
                j = i <= length(p) - 1 && entry >= p.keys[i] ? i + 1 : i # child index
                insert!(p.children, j + 1, rightnode)
                p.maxend = max(p.maxend, maxend)
                insert!(p.keys, j, median)

                # split when full
                if length(p) == B
                    leftnode, rightnode = split!(p)
                    median = minkey(rightnode)
                    maxend = max(leftnode.maxend, rightnode.maxend)
                else
                    hassplit = false
                end
            else
                p.maxend = max(p.maxend, last(entry))
            end
            parent = p.parent
        end

        if hassplit
            t.root = InternalNode{K, V, B}()
            push!(t.root.keys, median)
            push!(t.root.children, leftnode)
            push!(t.root.children, rightnode)
            t.root.maxend = maxend
            leftnode.parent = t.root
            rightnode.parent = t.root
        end
    else
        # travel back up the tree setting maxend values
        parent = node.parent
        while !isnull(parent)
            p = get(parent)
            p.maxend = max(p.maxend, last(entry))
            parent = p.parent
        end
    end

    return entry
end



# Deleting
# --------



function deletefirst!{K, V, B}(t::IntervalBTree{K, V, B}, first::K, last::K)
    return deletefirst!(t, Interval{K}(first, last))
end


function deletefirst!{K, V, B}(t::IntervalBTree{K, V, B}, key::(@compat Tuple{K, K}))
    return deletefirst!(t, Interval{K}(key[1], key[2]))
end


function deletefirst!{K, V, B}(t::IntervalBTree{K, V, B}, key::Interval{K})
    result = _deletefirst!(t.root, key)
    if result.keyfound
        t.n -= 1
    end

    # if the root has only one child, promote the child
    if isa(t.root, InternalNode) && length(t.root) == 1
        t.root = t.root.children[1]
        t.root.parent = Nullable{InternalNode{K, V, B}}()
    end

    return t
end


# Indicate what steps are need to account for an updated child.
immutable KeyFate
    value::Uint8
end

const KEYFATE_NONE         = KeyFate(0) # no changes
const KEYFATE_DELETE       = KeyFate(1) # delete the child node
const KEYFATE_DELETE_RIGHT = KeyFate(2) # delete the child's right sibling
const KEYFATE_UPDATE_LEFT  = KeyFate(3) # update the key separating the node
                                        # from its left sibling
const KEYFATE_UPDATE_RIGHT = KeyFate(4) # update the key separating the node
                                        # from its right sibling

# Information returned from a call to _delete
immutable DeleteResult
    keyfound::Bool # true if the key was found

    # Indicate what steps are need to account for an updated child. One of:
    fate::KeyFate
end


# Delete key from the subtree is present. Return a DeleteResult object.
function _deletefirst!{K, V, B}(t::InternalNode{K, V, B}, key::Interval{K})
    i = findidx(t, key)
    j = i <= length(t) - 1 && key >= t.keys[i] ? i + 1 : i # child index

    ans = _deletefirst!(t.children[j], key)

    if !ans.keyfound
        return DeleteResult(false, KEYFATE_NONE)
    end

    if ans.fate == KEYFATE_UPDATE_LEFT
        t.maxend = nodemaxend(t)
        if j > 1
            t.keys[j - 1] = minkey(t.children[j]);
            return DeleteResult(true, KEYFATE_NONE)
        else
            return DeleteResult(true, KEYFATE_UPDATE_LEFT)
        end
    elseif ans.fate == KEYFATE_UPDATE_RIGHT
        t.maxend = nodemaxend(t)
        if j < length(t)
            t.keys[j] = minkey(t.children[j+1])
        end

        if j > 1
            t.keys[j - 1] = minkey(t.children[j]);
            return DeleteResult(true, KEYFATE_NONE)
        else
            return DeleteResult(true, KEYFATE_UPDATE_LEFT)
        end
    elseif ans.fate == KEYFATE_DELETE || ans.fate == KEYFATE_DELETE_RIGHT
        deleteidx = ans.fate == KEYFATE_DELETE ? j : j + 1

        # deleteidx == 1 would only happen if we had only one child, but if
        # that were the case it would have been promoted.
        @assert deleteidx > 1

        splice!(t.keys, deleteidx - 1)
        splice!(t.children, deleteidx)
        t.maxend = nodemaxend(t)

        # not underfull
        minsize = div(B, 2)
        if length(t) >= minsize
            return DeleteResult(true, deleteidx == 2 ? KEYFATE_UPDATE_LEFT : KEYFATE_NONE)
        end

        # borrow right
        if !isnull(t.right)
            right = get(t.right)
            if sameparent(right, t) && length(right) > minsize
                splice!(right.keys, 1)
                push!(t.children, splice!(right.children, 1))
                push!(t.keys, minkey(t.children[end]))
                t.maxend = max(t.maxend, nodemaxend(t.children[end]))
                right.maxend = nodemaxend(right)
                t.children[end].parent = t

                return DeleteResult(true, KEYFATE_UPDATE_RIGHT)
            end
        end

        # borrow left
        if !isnull(t.left)
            left = get(t.left)
            if sameparent(left, t) && length(left) > minsize
                insert!(t.children, 1, pop!(left.children))
                pop!(left.keys)
                insert!(t.keys, 1, minkey(t.children[2]))
                t.maxend = max(t.maxend, nodemaxend(t.children[1]))
                left.maxend = nodemaxend(left)
                t.children[1].parent = t

                return DeleteResult(true, KEYFATE_UPDATE_LEFT)
            end
        end

        # merge with left
        if !isnull(t.left)
            left = get(t.left)
            if sameparent(left, t)
                merge!(left, t)
                left.right = t.right
                if !isnull(t.right)
                    get(t.right).left = left
                end

                return DeleteResult(true, KEYFATE_DELETE)
            end
        end

        # merge with right
        if !isnull(t.right)
            right = get(t.right)
            if sameparent(right, t)
                merge!(t, right)
                if !isnull(right.right)
                    get(right.right).left = t
                end
                t.right = right.right

                return DeleteResult(true, KEYFATE_DELETE_RIGHT)
            end
        end

        # Allow the root node to be underfull
        return DeleteResult(true, KEYFATE_NONE)
    else
        t.maxend = nodemaxend(t)
        return DeleteResult(true, KEYFATE_NONE)
    end
end


function _deletefirst!{K, V, B}(t::LeafNode{K, V, B}, key::Interval{K})
    i = findidx(t, key)

    # do nothing if the key isn't present
    if i < 1 || i > length(t) ||
        first(t.entries[i]) != key.first || last(t.entries[i]) != key.last
        return DeleteResult(false, KEYFATE_NONE)
    end

    splice!(t.entries, i)

    # This is the root node. Allow it to be empty.
    if isempty(t)
        return DeleteResult(true, KEYFATE_NONE)
    end

    minsize = div(B, 2)

    t.maxend = nodemaxend(t)

    # not underfull
    if length(t) >= minsize
        return DeleteResult(true, i == 1 ? KEYFATE_UPDATE_LEFT : KEYFATE_NONE)
    end

    # borrow right
    if !isnull(t.right)
        right = get(t.right)
        if sameparent(right, t) && length(right) > minsize
            push!(t.entries, splice!(right.entries, 1))
            t.maxend = max(t.maxend, last(t.entries[end]))
            right.maxend = nodemaxend(right)

            return DeleteResult(true, KEYFATE_UPDATE_RIGHT)
        end
    end

    # borrow left
    if !isnull(t.left)
        left = get(t.left)
        if sameparent(left, t) && length(left) > minsize
            insert!(t.entries, 1, pop!(left.entries))
            t.maxend = max(t.maxend, last(t.entries[1]))
            left.maxend = nodemaxend(left)

            return DeleteResult(true, KEYFATE_UPDATE_LEFT)
        end
    end

    # merge with left
    if !isnull(t.left)
        left = get(t.left)
        if sameparent(left, t)
            merge!(left, t)
            left.right = t.right
            if !isnull(t.right)
                get(t.right).left = left
            end
            return DeleteResult(true, KEYFATE_DELETE)
        end
    end

    # merge with right
    if !isnull(t.right)
        right = get(t.right)
        if sameparent(right, t)
            merge!(t, right)
            if !isnull(right.right)
                get(right.right).left = t
            end
            t.right = right.right
            return DeleteResult(true, KEYFATE_DELETE_RIGHT)
        end
    end

    # This must be the root node. Allow it to be underfull.
    return DeleteResult(true, KEYFATE_NONE)
end


# Join two leaf nodes into one.
function merge!{K, V, B}(left::LeafNode{K, V, B}, right::LeafNode{K, V, B})
    leftlen, rightlen = length(left), length(right)
    @assert leftlen + rightlen <= B
    resize!(left.entries, leftlen + rightlen)
    copy!(left.entries, leftlen+1, right.entries, 1, length(right.entries))
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

function findidx{K, V, B}(t::LeafNode{K, V, B}, key::AbstractInterval{K})
    if isempty(t.entries)
        return 0
    end
    return searchsortedfirst(t.entries, key)
end

function findidx{K, V, B}(t::InternalNode{K, V, B}, key::AbstractInterval{K})
    if isempty(t.keys)
        return 0
    end
    i = searchsortedfirst(t.keys, key)
    return min(i, length(t.keys))
end

function Base.haskey{K, V, B}(t::LeafNode{K, V, B}, key::AbstractInterval{K})
    i = findidx(t, key)
    return 1 <= i <= length(t) && first(t.entries[i]) == key.first &&
           last(t.entries[i]) == key.last
end


function Base.haskey{K, V, B}(t::InternalNode{K, V, B}, key::AbstractInterval{K})
    i = findidx(t, key)
    if i <= length(t) - 1 && key >= t.keys[i]
        return haskey(t.children[i+1], key)
    else
        return haskey(t.children[i], key)
    end
end


function Base.haskey{K, V, B}(t::IntervalBTree{K, V, B}, key0::(@compat Tuple{Any, Any}))
    key = Interval{K}(key0[1], key0[2])
    return haskey(t.root, key)
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
function intersects{K}(key1::AbstractInterval{K}, key2::AbstractInterval{K})
    return first(key1) <= last(key2) && first(key2) <= last(key1)
end


# Return true if the tree has an intersection with the given position
function hasintersection{K, V, B}(t::IntervalBTree{K, V, B}, query)
    return hasintersection(t.root, convert(K, query))
end


function hasintersection{K, V, B}(t::InternalNode{K, V, B}, query::K)
    if isempty(t) || t.maxend < query
        return false
    end

    for (i, child) in enumerate(t.children)
        if child.maxend >= query && (i == 1 || t.keys[i-1].first <= query)
            if hasintersection(child, query)
                return true
            end
        elseif minstart(child) > query
            break
        end
    end

    return false
end


function hasintersection{K, V, B}(t::LeafNode{K, V, B}, query::K)
    if isempty(t) || t.maxend < query
        return false
    end

    for i in 1:length(t)
        if first(t.entries[i]) <= query <= last(t.entries[i])
            return true
        elseif query < first(t.entries[i])
            break
        end
    end

    return false
end


# Represent an intersection in an IntervalTree by pointing to a LeafNode
# and an index within that leaf node. No intersection is represented with
# index == 0.
immutable Intersection{K, V, B}
    index::Int
    node::LeafNode{K, V, B}

    Intersection(index, node) = new(index, node)
    Intersection() = new(0)
end


# Find the first interval in the tree that intersects the query and return
# as a (leafnode, index) pair, indicating that leafnode.keys[index] intersects.
# If no intersection is found, index is 0 and leafnode is the last node
# searched.
function firstintersection{K, V, B}(t::IntervalBTree{K, V, B}, query::AbstractInterval{K})
    return firstintersection(t.root, query)
end


function firstintersection{K, V, B}(t::InternalNode{K, V, B}, query::AbstractInterval{K})
    if isempty(t) || t.maxend < first(query)
        return Intersection{K, V, B}()
    end

    for (i, child) in enumerate(t.children)
        if child.maxend >= first(query) && (i == 1 || t.keys[i-1].first <= last(query))
            intersection = firstintersection(child, query)
            if intersection.index > 0
                return intersection
            end
        elseif minstart(child) > last(query)
            break
        end
    end

    return Intersection{K, V, B}()
end


function firstintersection{K, V, B}(t::LeafNode{K, V, B}, query::AbstractInterval{K})
    if isempty(t) || t.maxend < first(query)
        return Intersection{K, V, B}()
    end

    for i in 1:length(t)
        if intersects(t.entries[i], query)
            return Intersection{K, V, B}(i, t)
        elseif query.last < first(t.entries[i])
            break
        end
    end

    return Intersection{K, V, B}()
end


# Find the first key with an end point >= query
function firstfrom{K, V, B}(t::IntervalBTree{K, V, B}, query::K)
    return firstfrom(t.root, query)
end


function firstfrom{K, V, B}(t::InternalNode{K, V, B}, query::K)
    if isempty(t)
        return (t, 0)
    end

    for (i, child) in enumerate(t.children)
        if child.maxend >= query
            return firstfrom(child, query)
        end
    end

    return (t, 0)
end


function firstfrom{K, V, B}(t::LeafNode{K, V, B}, query::K)
    if isempty(t)
        return (t, 0)
    end

    for i in 1:length(t)
        if last(t.entries[i]) >= query
            return (t, i)
        end
    end

    return (t, 0)
end



# If query intersects node.keys[i], return the next intersecting key as
# a (leafnode, index) pair.
function nextintersection{K, V, B}(t::LeafNode{K, V, B}, i::Integer,
                                   query::AbstractInterval{K})
    j = i + 1
    while true
        while j <= length(t)
            if intersects(t.entries[j], query)
                return Intersection{K, V, B}(j, t)
            end
            j += 1
        end
        j = 1
        if isnull(t.right)
            break
        end
        t = get(t.right)
        if minstart(t) > last(query)
            break
        end
    end

    return Intersection{K, V, B}()
end


immutable IntervalIntersectionIterator{K, V, B}
    t::IntervalBTree{K, V, B}
    query::AbstractInterval{K}
end


# Intersect an interval tree t with a single interval, returning an iterator
# over the intersecting (key, value) pairs in t.
function Base.intersect{K, V, B}(t::IntervalBTree{K, V, B}, query0::(@compat Tuple{Any, Any}))
    query = Interval{K}(query0[1], query0[2])
    return intersect(t, query)
end


function Base.intersect{K, V, B}(t::IntervalBTree{K, V, B}, first::K, last::K)
    query = Interval{K}(first, last)
    return intersect(t, query)
end


function Base.intersect{K, V, B}(t::IntervalBTree{K, V, B}, query::AbstractInterval{K})
    return IntervalIntersectionIterator(t, query)
end


function Base.start{K, V, B}(it::IntervalIntersectionIterator{K, V, B})
    return firstintersection(it.t, it.query)
end


function Base.next{K, V, B}(it::IntervalIntersectionIterator{K, V, B},
                            state::Intersection{K, V, B})
    entry = state.node.entries[state.index]
    state = nextintersection(state.node, state.index, it.query)
    return entry, state
end


function Base.done{K, V, B}(it::IntervalIntersectionIterator{K, V, B},
                            state::Intersection{K, V, B})
    return state.index == 0
end


type IntersectionIterator{K, V1, B1, V2, B2}
    t1::IntervalBTree{K, V1, B1}
    t2::IntervalBTree{K, V2, B2}

    # if true, use successive intersection, if false iterative.
    successive::Bool

    # true if done
    isdone::Bool

    # successive intersection state
    intersection::Intersection{K, V2, B2}
    t1_state::IntervalBTreeIteratorState{K, V1, B1}
    t1_value::V1

    # iterative intersection state
    u::Nullable{LeafNode{K, V1, B1}}
    v::Nullable{LeafNode{K, V2, B2}}
    w::Nullable{LeafNode{K, V2, B2}}
    i::Int
    j::Int
    k::Int

    function IntersectionIterator(t1, t2, successive::Bool)
        return new(t1, t2, successive)
    end
end


function Base.start{K, V1, B1, V2, B2}(it::IntersectionIterator{K, V1, B1, V2, B2})
    it.isdone = true
    if it.successive
        # Successive Query Intersection: intersect by repeatedly performing
        # queries against on tree ot the other.

        t1_state = start(it.t1)
        while !done(it.t1, t1_state)
            t1_value, t1_state = next(it.t1, t1_state)
            intersection = firstintersection(it.t2, t1_value)
            if intersection.index != 0
                it.intersection = intersection
                it.t1_state = t1_state
                it.t1_value = t1_value
                it.isdone = false
                return nothing
            end
        end
    else
        # Iterative Intersection: Intersect by iterating through the two
        # collections in unison.
        #
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

        it.u = isempty(it.t1) ? Nullable{LeafNode{K, V1, B1}}() : firstleaf(it.t1)
        it.v = isempty(it.t2) ? Nullable{LeafNode{K, V2, B2}}() : firstleaf(it.t2)
        it.w = it.v
        it.i, it.j, it.k = 1, 1, 1
        nextintersection!(it)
    end
    return nothing
end


function nextintersection!{K, V1, B1, V2, B2}(it::IntersectionIterator{K, V1, B1, V2, B2})
    u, v, w, i, j, k = it.u, it.v, it.w, it.i, it.j, it.k
    it.isdone = true
    while !isnull(u) && !isnull(v)
        unode = get(u)
        vnode = get(v)
        if isnull(w)
            u, i = nextleafkey(unode, i)
            w, k = v, j
            continue
        end
        wnode = get(w)

        if last(unode.entries[i]) < first(vnode.entries[j])
            u, i = nextleafkey(unode, i)
        elseif last(vnode.entries[j]) < first(unode.entries[i])
            v, j = nextleafkey(vnode, j)
        elseif first(wnode.entries[k]) <= last(unode.entries[i])
            if intersects(unode.entries[i], wnode.entries[k])
                it.isdone = false
                break
            end
            w, k = nextleafkey(wnode, k)
        else
            u, i = nextleafkey(unode, i)
            w, k = v, j
        end
    end

    it.u = u
    it.v = v
    it.w = w
    it.i = i
    it.j = j
    it.k = k
    return
end


function Base.next{K, V1, B1, V2, B2}(it::IntersectionIterator{K, V1, B1, V2, B2}, state)
    if it.successive
        intersection = it.intersection
        entry = intersection.node.entries[intersection.index]
        value = (it.t1_value, entry)
        intersection = nextintersection(intersection.node, intersection.index, it.t1_value)

        t1 = it.t1
        t2 = it.t2
        t1_state = it.t1_state
        t1_value = it.t1_value
        while intersection.index == 0 && !done(t1, t1_state)
            t1_value, t1_state = next(t1, t1_state)
            intersection = firstintersection(t2, t1_value)
        end
        it.isdone = intersection.index == 0
        it.intersection = intersection
        it.t1_state = t1_state
        it.t1_value = t1_value
    else
        u = get(it.u)
        w = get(it.w)
        value = (u.entries[it.i], w.entries[it.k])
        it.w, it.k = nextleafkey(w, it.k)
        nextintersection!(it)
    end
    return (value, nothing)
end


function Base.done{K, V1, B1, V2, B2}(it::IntersectionIterator{K, V1, B1, V2, B2}, state)
    return it.isdone
end


# Intersect two interval trees, returning an iterator yielding values of the
# form:
#   ((key1, value1), (key2, value2))
#
# Where key1 is from the first tree and key2 from the second, and they
# intersect.
function Base.intersect{K, V1, B1, V2, B2}(t1::IntervalBTree{K, V1, B1},
                                           t2::IntervalBTree{K, V2, B2};
                                           method=:auto)
    # We decide heuristically which intersection algorithm to use.
    n = length(t1)
    m = length(t2)

    iterative_cost  = n + m
    successive_cost = n * log(1 + m)

    if method == :auto
        iterative_cost  = n + m
        successive_cost = n * log(1 + m)
        if iterative_cost < successive_cost
            return IntersectionIterator{K, V1, B1, V2, B2}(t1, t2, false)
        else
            return IntersectionIterator{K, V1, B1, V2, B2}(t1, t2, true)
        end
    elseif method == :successive
        return IntersectionIterator{K, V1, B1, V2, B2}(t1, t2, true)
    elseif method == :iterative
        return IntersectionIterator{K, V1, B1, V2, B2}(t1, t2, false)
    else
        error("No such intersection method: $method")
    end
end


# Diagnostics
# -----------


# Dumb tree printing, useful only for debuging.
#function showtree(io::IO, t::IntervalBTree)
    #showtree(io, t.root, 0)
#end


#function showtree(t::IntervalBTree)
    #showtree(STDOUT, t)
#end


#function showtree(io::IO, t::InternalNode, indent::Int)
    #for (i, child) in enumerate(t.children)
        #showtree(io, child, indent+1)
        #if i <= length(t.keys)
            #print(io, repeat("  ", indent), t.keys[i], ":\n")
        #end
    #end
#end


#function showtree(io::IO, t::LeafNode, indent::Int)
    #for (k, v) in zip(t.keys, t.values)
        #print(io, repeat("  ", indent), k, ": ", v, "\n")
    #end
#end

include("map.jl")

end

