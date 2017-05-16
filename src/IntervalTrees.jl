__precompile__()

module IntervalTrees

export
    IntervalTree,
    IntervalMap,
    AbstractInterval,
    Interval,
    IntervalValue,
    depth,
    hasintersection,
    from,
    value

import Compat: @compat

include("slice.jl")


"""
An `AbstractInterval{T}` must have a `first` and `last` function each returning
a value of type `T`, and `first(i) <= last(i)` must always be true.
"""
@compat abstract type AbstractInterval{T} end


function basic_isless(u::AbstractInterval, v::AbstractInterval)
    return first(u) < first(v) || (first(u) == first(v) && last(u) < last(v))
end


function Base.isless(u::AbstractInterval, v::AbstractInterval)
    return basic_isless(u, v)
end


"""
A basic interval.
"""
immutable Interval{T} <: AbstractInterval{T}
    first::T
    last::T
end
Base.convert{T}(::Type{Interval{T}}, range::Range{T}) = Interval(first(range), last(range))
Interval{T}(range::Range{T}) = convert(Interval{T}, range)
Base.first{T}(i::Interval{T}) = i.first
Base.last{T}(i::Interval{T}) = i.last


"""
An interval with some associated data.
"""
immutable IntervalValue{K, V} <: AbstractInterval{K}
    first::K
    last::K
    value::V
end
IntervalValue{K, V}(range::Range{K}, value::V) = IntervalValue(first(range), last(range), value)

valtype{K,V}(::Type{IntervalValue{K,V}}) = V

Base.first{K, V}(i::IntervalValue{K, V}) = i.first
Base.last{K, V}(i::IntervalValue{K, V}) = i.last
value{K, V}(i::IntervalValue{K, V}) = i.value

Base.print(io::IO, x::Interval) = print(io, "\n($(first(x)),$(last(x)))")
function Base.show(io::IO, x::Interval)
    show(io, typeof(x))
    print(io, x)
end

Base.print(io::IO, x::IntervalValue) = print(io, "\n($(first(x)),$(last(x))) => $(value(x))")
function Base.show(io::IO, x::IntervalValue)
    show(io, typeof(x))
    print(io, x)
end

# Each of these types is indexes by K, V, B, where
#   K : Interval type. Intervals are represented as (K, K) tuples.
#   V : Interval value type. This an interval type that may have associated
#        data. It must have `first`, `last`, and `isless` methods.
#   B : Integer giving the B-tree order.

@compat abstract type Node{K, V, B} end


type InternalNode{K, V, B} <: Node{K, V, B}
    # Internal nodes are keyed by the minimum interval in the right subtree.  We
    # need internal node keys to be intervals themselves, since ordering by
    # start value alone only works if start values are unique. We don't
    # force that restriction, so we must break ties in order to split nodes that
    # are full of intervals with the same start value.
    keys::Slice{Interval{K}, B}

    # Maximum end ofd this node
    maxend::K

    # Maximum child subtree end-points
    maxends::Slice{K, B}

    children::Slice{Node{K, V, B}, B}

    parent::Nullable{InternalNode{K, V, B}}

    # Sibling/cousin pointers.
    left::Nullable{InternalNode{K, V, B}}
    right::Nullable{InternalNode{K, V, B}}

    function InternalNode()
        t = new(Slice{Interval{K}, B}(), zero(K), Slice{K, B}(), Slice{Node{K, V, B}, B}(),
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
    entries::Vector{V}
    keys::Vector{Interval{K}}
    count::UInt32 # number of stored entries

    # maximum interval end-point in this leaf node.
    maxend::K

    parent::Nullable{InternalNode{K, V, B}}

    # Sibling/cousin pointers.
    left::Nullable{LeafNode{K, V, B}}
    right::Nullable{LeafNode{K, V, B}}

    function LeafNode()
        return new(
            Array{V}(B), Array{Interval{K}}(B), 0,
            zero(K),
            Nullable{InternalNode{K, V, B}}(),
            Nullable{LeafNode{K, V, B}}(),
            Nullable{LeafNode{K, V, B}}())
    end
end


function minstart(t::LeafNode)
    return first(t.keys[1])
end


function Base.resize!{K, V, B}(leaf::LeafNode{K, V, B}, n)
    @assert 0 <= n <= B
    leaf.count = n
end


function Base.splice!{K, V, B}(leaf::LeafNode{K, V, B}, i::Integer)
    slice_splice!(leaf.keys, leaf.count, i)
    x, leaf.count = slice_splice!(leaf.entries, leaf.count, i)
    return x
end


function Base.insert!{K, V, B}(leaf::LeafNode{K, V, B}, i::Integer, value::V)
    slice_insert!(leaf.keys, leaf.count, i, Interval{K}(first(value), last(value)))
    slice_insert!(leaf.entries, leaf.count, i, value)
    leaf.count += 1
end


function Base.push!{K, V, B}(leaf::LeafNode{K, V, B}, value::V)
    leaf.count += 1
    leaf.entries[leaf.count] = value
    leaf.keys[leaf.count] = Interval{K}(first(value), last(value))
end


function Base.pop!{K, V, B}(leaf::LeafNode{K, V, B})
    x = leaf.entries[leaf.count]
    leaf.count -= 1
    return x
end


# iterate through entries in a leaf node
function Base.start(leaf::LeafNode)
    return 1
end


function Base.next(leaf::LeafNode, i::Int)
    return (leaf.entries[i], i + 1)
end


function Base.done(leaf::LeafNode, i::Int)
    return i > length(leaf)
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

        numleaves = cld(n, B - 2)
        leaves = [LeafNode{K, V, B}() for _ in 1:numleaves]

        maxends = Vector{K}(numleaves)
        minkeys = Vector{Interval{K}}(numleaves)

        # divy up the keys and values among the leaves
        keys_per_leaf = cld(n, numleaves)
        for i in 1:numleaves
            u = (i - 1) * keys_per_leaf + 1
            v = min(n, i * keys_per_leaf)
            minkeys[i] = Interval{K}(first(entries[u]), last(entries[u]))
            maxends[i] = last(entries[u])
            for j in u:v
                push!(leaves[i], entries[j])
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
            numparents = cld(length(children), B - 2)
            parents = [InternalNode{K, V, B}() for _ in 1:numparents]

            # divy up children among parents
            children_per_parent = cld(length(children), numparents)
            for i in 1:numparents
                u = (i - 1) * keys_per_leaf + 1
                v = min(length(children), i * keys_per_leaf)
                maxend = maxends[u]
                for j in u:v
                    push!(parents[i].children, children[j])
                    push!(parents[i].maxends, maxends[j])
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
@compat const IntervalTree{K, V} = IntervalBTree{K, V, 64}

# Show

function Base.show(io::IO, it::IntervalTree)
    show(io, typeof(it))
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
    return t.count
end


function Base.isempty(t::IntervalBTree)
    return t.n == 0
end


function Base.isempty(t::InternalNode)
    return isempty(t.children)
end


function Base.isempty(t::LeafNode)
    return t.count == 0
end


# Iterating
# ---------


type IntervalBTreeIteratorState{K, V, B}
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
        state.i += 1
    else
        state.leaf = leaf.right
        state.i = 1
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

Base.eltype{K,V,B}(::Type{IntervalFromIterator{K,V,B}}) = V
Base.iteratorsize{K,V,B}(::Type{IntervalFromIterator{K,V,B}}) = Base.SizeUnknown()

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
    resize!(right, m - div(m, 2))
    copy!(right.entries, 1, left.entries, div(m, 2) + 1, length(right))
    copy!(right.keys, 1, left.keys, div(m, 2) + 1, length(right))
    resize!(left, div(m, 2))

    left.maxend = last(left.keys[1])
    for entry in left
        left.maxend = max(left.maxend, last(entry))
    end

    right.maxend = last(right.keys[1])
    for entry in right
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
    resize!(right.maxends, m - div(m, 2))
    resize!(right.keys, m - div(m, 2) - 1)

    copy!(right.children, 1, left.children, div(m, 2)+1, length(right.children))
    copy!(right.maxends, 1, left.maxends, div(m, 2)+1, length(right.maxends))
    copy!(right.keys, 1, left.keys, div(m, 2)+1, length(right.keys))

    resize!(left.children, div(m, 2))
    resize!(left.maxends, div(m, 2))
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
    for i in 1:length(t)
        maxend = max(maxend, last(t.keys[i]))
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


# Find the root node
function root(t::Node)
    while !isnull(t.parent)
        t = get(t.parent)
    end
    return t
end


# TODO: Having to construct Nullables here is killng us.
# How can we avoid that?
#
function nextleafkey{K, V, B}(t::LeafNode{K, V, B}, i::Int)
    if i < length(t)
        return (Nullable{LeafNode{K, V, B}}(t), i + 1)
    else
        return (t.right, 1)
    end
end

# This version of nextleafkey is used in nextintersection at aggressively avoid
# allocation, and dereferecing nullables more than once.
macro nextleafkey(leaf, nleaf, i)
    quote
        if $(esc(i)) < length($(esc(leaf)))
            $(esc(i)) += 1
        else
            $(esc(i)) = 1
            $(esc(nleaf)) = $(esc(leaf)).right
        end
    end
end



# Find the minimum interval in a subtree
function minkey(t::InternalNode)
    return minkey(t.children[1])
end

function minkey{K, V, B}(t::LeafNode{K, V, B})
    return t.keys[1]
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
        first(node.keys[i]) == first(entry) && last(node.keys[i]) == last(entry)
        if update
            node.entries[i] = entry
            return entry
        else
            return node.entries[i]
        end
    end

    insert!(node, i, entry)
    if length(node) == 1 || last(entry) > node.maxend
        node.maxend = last(entry)
    end
    t.n += 1

    # split when full
    if length(node) == B
        maxend = node.maxend
        leftleaf, rightleaf = split!(node)
        median = rightleaf.keys[1]

        # travel back up the tree setting maxend values and splitting as needed
        parent = node.parent
        leftnode = leftleaf
        child = leftleaf
        rightnode = rightleaf
        hassplit = true
        while !isnull(parent)
            p = get(parent)
            if hassplit
                i = findidx(p, entry) # key index
                j = i <= length(p) - 1 && entry >= p.keys[i] ? i + 1 : i # child index
                p.maxends[j] = p.children[j].maxend
                insert!(p.children, j + 1, rightnode)
                insert!(p.maxends, j + 1, p.children[j+1].maxend)
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
                p.maxends[findfirst(p.children, child)] = child.maxend
            end
            child = p
            parent = p.parent
        end

        if hassplit
            t.root = InternalNode{K, V, B}()
            push!(t.root.keys, median)
            push!(t.root.children, leftnode)
            push!(t.root.children, rightnode)
            t.root.maxend = maxend
            push!(t.root.maxends, leftnode.maxend)
            push!(t.root.maxends, rightnode.maxend)
            leftnode.parent = t.root
            rightnode.parent = t.root
        end
    else
        # travel back up the tree setting maxend values
        parent = node.parent
        child = node
        while !isnull(parent)
            p = get(parent)
            p.maxend = max(p.maxend, last(entry))
            p.maxends[findfirst(p.children, child)] = child.maxend
            parent = p.parent
            child = p
        end
    end

    return entry
end



# Deleting
# --------



function deletefirst!{K, V, B}(t::IntervalBTree{K, V, B}, first::K, last::K)
    return deletefirst!(t, Interval{K}(first, last))
end


function deletefirst!{K, V, B}(t::IntervalBTree{K, V, B}, key::Tuple{K, K})
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
    value::UInt8
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


# Delete key from the subtree if present. Return a DeleteResult object.
function _deletefirst!{K, V, B}(t::InternalNode{K, V, B}, key::Interval{K})
    i = findidx(t, key)
    j = i <= length(t) - 1 && key >= t.keys[i] ? i + 1 : i # child index

    ans = _deletefirst!(t.children[j], key)

    if !ans.keyfound
        return DeleteResult(false, KEYFATE_NONE)
    end

    if ans.fate == KEYFATE_UPDATE_LEFT
        t.maxend = nodemaxend(t)
        t.maxends[j] = t.children[j].maxend
        if j > 1
            t.maxends[j-1] = t.children[j-1].maxend
            t.keys[j-1] = minkey(t.children[j]);
            return DeleteResult(true, KEYFATE_NONE)
        else
            return DeleteResult(true, KEYFATE_UPDATE_LEFT)
        end
    elseif ans.fate == KEYFATE_UPDATE_RIGHT
        t.maxend = nodemaxend(t)
        t.maxends[j] = t.children[j].maxend
        if j < length(t)
            t.maxends[j+1] = t.children[j+1].maxend
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
        splice!(t.maxends, deleteidx)
        t.maxend = nodemaxend(t)
        if ans.fate == KEYFATE_DELETE
            t.maxends[j-1] = t.children[j-1].maxend
        elseif ans.fate == KEYFATE_DELETE_RIGHT
            t.maxends[j] = t.children[j].maxend
        end

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
                push!(t.maxends, splice!(right.maxends, 1))
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
                insert!(t.maxends, 1, pop!(left.maxends))
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
    else # KEYFATE_NONE
        t.maxend = nodemaxend(t)
        t.maxends[j] = t.children[j].maxend
        return DeleteResult(true, KEYFATE_NONE)
    end
end


function _deletefirst!{K, V, B}(t::LeafNode{K, V, B}, key::Interval{K})
    i = findidx(t, key)

    # do nothing if the key isn't present
    if i < 1 || i > length(t) ||
        first(t.keys[i]) != key.first || last(t.keys[i]) != key.last
        return DeleteResult(false, KEYFATE_NONE)
    end

    splice!(t, i)

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
            push!(t, splice!(right, 1))
            t.maxend = max(t.maxend, last(t.keys[t.count]))
            right.maxend = nodemaxend(right)

            return DeleteResult(true, KEYFATE_UPDATE_RIGHT)
        end
    end

    # borrow left
    if !isnull(t.left)
        left = get(t.left)
        if sameparent(left, t) && length(left) > minsize
            insert!(t, 1, pop!(left))
            t.maxend = max(t.maxend, last(t.keys[1]))
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
    resize!(left, leftlen + rightlen)
    copy!(left.entries, leftlen+1, right.entries, 1, length(right))
    copy!(left.keys, leftlen+1, right.keys, 1, length(right))
    left.maxend = nodemaxend(left)
    return
end


# Join two internal nodes into one
function merge!{K, V, B}(left::InternalNode{K, V, B}, right::InternalNode{K, V, B})
    leftlen, rightlen = length(left), length(right)
    @assert length(left) + length(right) <= B
    resize!(left.keys, leftlen + rightlen - 1)
    resize!(left.children, leftlen + rightlen)
    resize!(left.maxends, leftlen + rightlen)
    copy!(left.children, leftlen+1, right.children, 1, length(right.children))
    copy!(left.maxends, leftlen+1, right.maxends, 1, length(right.maxends))
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
    if isempty(t)
        return 0
    end
    # TODO: search keys
    return searchsortedfirst(t.entries, key, 1, Int(t.count),
                             Base.ord(basic_isless, identity, false))
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
    return 1 <= i <= length(t) && first(t.keys[i]) == key.first &&
           last(t.keys[i]) == key.last
end


function Base.haskey{K, V, B}(t::InternalNode{K, V, B}, key::AbstractInterval{K})
    i = findidx(t, key)
    if i <= length(t) - 1 && key >= t.keys[i]
        return haskey(t.children[i+1], key)
    else
        return haskey(t.children[i], key)
    end
end


function Base.haskey{K, V, B}(t::IntervalBTree{K, V, B}, key0::Tuple{Any, Any})
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
@inline function intersects{K}(key1::AbstractInterval{K}, key2::AbstractInterval{K})
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
        if first(t.keys[i]) <= query <= last(t.keys[i])
            return true
        elseif query < first(t.keys[i])
            break
        end
    end

    return false
end


# Represent an intersection in an IntervalTree by pointing to a LeafNode
# and an index within that leaf node. No intersection is represented with
# index == 0.
type Intersection{K, V, B}
    index::Int
    node::LeafNode{K, V, B}

    Intersection(index, node) = new(index, node)
    Intersection() = new(0)
end


# Find the first interval in the tree that intersects the query and return
# as a (leafnode, index) pair, indicating that leafnode.keys[index] intersects.
# If no intersection is found, index is 0 and leafnode is the last node
# searched.
function firstintersection!{K, V, B}(t::IntervalBTree{K, V, B},
                                     query::AbstractInterval{K},
                                     lower::Nullable{V},
                                     out::Intersection{K, V, B})
    return firstintersection!(t.root, query, lower, out)
end


function firstintersection!{K, V, B}(t::InternalNode{K, V, B},
                                     query::AbstractInterval{K},
                                     lower::Nullable{V},
                                     out::Intersection{K, V, B})
    if isempty(t) || t.maxend < first(query)
        out.index = 0
        return
    end

    (query_first, query_last) = (first(query), last(query))

    i = isnull(lower) ? 1 : 1 + searchsortedlast(t.keys, get(lower))

    while i <= length(t.children)
        if i > 1 && unsafe_getindex(t.keys, i-1).first > query_last
            break
        end

        if t.maxends[i] >= query_first
            firstintersection!(unsafe_getindex(t.children, i), query, lower, out)
            if out.index > 0
                return
            end
        end
        i += 1
    end

    out.index = 0
    return
end


function firstintersection!{K, V, B}(t::LeafNode{K, V, B},
                                     query::AbstractInterval{K},
                                     lower::Nullable{V},
                                     out::Intersection{K, V, B})
    if isempty(t) || t.maxend < first(query)
        out.index = 0
        return
    end

    # TODO: search keys
    i = isnull(lower) ? 1 :
        1 + searchsortedlast(t.entries, get(lower), 1, Int(t.count),
                             Base.ord(basic_isless, identity, false))

    while i <= length(t)
        if intersects(t.entries[i], query)
            out.index = i
            out.node = t
            return
        elseif query.last < first(t.keys[i])
            break
        end
        i += 1
    end

    out.index = 0
    return
end


function firstintersection{K, V, B}(t::InternalNode{K, V, B},
                                    query::Interval{K},
                                    lower::Nullable{Interval{K}})
    if isempty(t) || t.maxend < first(query)
        return (Nullable{LeafNode{K, V, B}}(), 1)
    end

    i = isnull(lower) ? 1 : 1 + searchsortedlast(t.keys, get(lower))

    while i <= length(t.children)
        if i > 1 && unsafe_getindex(t.keys, i-1).first > last(query)
            break
        end

        if t.maxends[i] >= first(query)
            return firstintersection(unsafe_getindex(t.children, i), query,
                                     lower)
        end
        i += 1
    end

    return (Nullable{LeafNode{K, V, B}}(), 1)
end


function firstintersection{K, V, B}(t::LeafNode{K, V, B}, query::Interval{K},
                                    lower::Nullable{Interval{K}})

    if isempty(t) || t.maxend < first(query)
        return (Nullable{LeafNode{K, V, B}}(), 1)
    end

    i = isnull(lower) ? 1 : 1 + searchsortedlast(t.keys, get(lower))
    while i <= length(t)
        if intersects(t.keys[i], query)
            return (Nullable(t), i)
        elseif last(query) < first(t.keys[i])
            break
        end
        i += 1
    end

    return (Nullable{LeafNode{K, V, B}}(), 1)
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
        if last(t.keys[i]) >= query
            return (t, i)
        end
    end

    return (t, 0)
end



# If query intersects node.keys[i], return the next intersecting key as
# a (leafnode, index) pair.
function nextintersection!{K, V, B}(t::LeafNode{K, V, B}, i::Integer,
                                    query::AbstractInterval{K},
                                    out::Intersection{K, V, B})
    j = i + 1
    while true
        while j <= length(t)
            if intersects(t.keys[j], query)
                out.index = j
                out.node = t
                return
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

        # When we hit a leaf node with no possible intersections, we start over
        # from the top of the leaf. This is a heuristic to avoid linear search
        # behavior in the presence of extremely long intervals.
        if t.maxend < first(query)
            return firstintersection!(root(t), query,
                                      Nullable(t.entries[t.count]), out)
        end
    end

    out.index = 0
    return
end


type IntervalIntersectionIterator{K, V, B}
    intersection::Intersection{K, V, B}
    t::IntervalBTree{K, V, B}
    query::AbstractInterval{K}

    function IntervalIntersectionIterator(intersection, t, query)
        return new(intersection, t, query)
    end

    function IntervalIntersectionIterator()
        return new(Intersection{K, V, B}())
    end
end

Base.eltype{K,V,B}(::Type{IntervalIntersectionIterator{K,V,B}}) = V
Base.iteratorsize{K,V,B}(::Type{IntervalIntersectionIterator{K,V,B}}) = Base.SizeUnknown()

# Intersect an interval tree t with a single interval, returning an iterator
# over the intersecting (key, value) pairs in t.
function Base.intersect{K, V, B}(t::IntervalBTree{K, V, B}, query0::Tuple{Any, Any})
    query = Interval{K}(query0[1], query0[2])
    return intersect(t, query)
end


function Base.intersect{K, V, B}(t::IntervalBTree{K, V, B}, first::K, last::K)
    query = Interval{K}(first, last)
    return intersect(t, query)
end


function Base.intersect{K, V, B}(t::IntervalBTree{K, V, B}, query::AbstractInterval{K})
    return IntervalIntersectionIterator{K, V, B}(Intersection{K, V, B}(), t, query)
end


function Base.start{K, V, B}(it::IntervalIntersectionIterator{K, V, B})
    return firstintersection!(it.t, it.query, Nullable{V}(), it.intersection)
end


function Base.next{K, V, B}(it::IntervalIntersectionIterator{K, V, B}, ::Void)
    intersection = it.intersection
    entry = intersection.node.entries[intersection.index]
    nextintersection!(intersection.node, intersection.index,
                      it.query, intersection)
    return entry, nothing
end


function Base.done{K, V, B}(it::IntervalIntersectionIterator{K, V, B}, ::Void)
    return it.intersection.index == 0
end


type IntersectionIterator{K, V1, B1, V2, B2}
    t1::IntervalBTree{K, V1, B1}
    t2::IntervalBTree{K, V2, B2}

    # if true, use successive intersection, if false iterative.
    successive::Bool

    # true if done
    isdone::Bool

    # intersection state
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

Base.eltype{K,V1,B1,V2,B2}(::Type{IntersectionIterator{K,V1,B1,V2,B2}}) = Tuple{V1,V2}
Base.iteratorsize{K,V1,B1,V2,B2}(::Type{IntersectionIterator{K,V1,B1,V2,B2}}) = Base.SizeUnknown()

function Base.start{K, V1, B1, V2, B2}(it::IntersectionIterator{K, V1, B1, V2, B2})
    it.isdone = true
    if it.successive
        it.u = isempty(it.t1) ? Nullable{LeafNode{K, V1, B1}}() : firstleaf(it.t1)
        it.w = isempty(it.t2) ? Nullable{LeafNode{K, V2, B2}}() : firstleaf(it.t2)
        it.i, it.k = 1, 1
        successive_nextintersection!(it)
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
        iterative_nextintersection!(it)
    end
    return nothing
end


function iterative_nextintersection!{K, V1, B1, V2, B2}(
                        it::IntersectionIterator{K, V1, B1, V2, B2})
    u, v, w, i, j, k = it.u, it.v, it.w, it.i, it.j, it.k

    it.isdone = true

    while true
        if isnull(u)
            return
        end
        unode = get(u)
        ukey = unode.keys[i]

        if isnull(v)
            return
        end
        vnode = get(v)
        vkey = vnode.keys[j]

        # find next intersection w
        while !isnull(w)
            wnode = get(w)
            wkey = wnode.keys[k]

            if first(wkey) <= last(ukey)
                if first(ukey) <= last(wkey)
                    it.isdone = false
                    break
                end
                @nextleafkey(wnode, w, k)
            else
                break
            end
        end

        # intersection was found
        if !it.isdone
            break
        end

        # if no intersection found, advance u and start again
        @nextleafkey(unode, u, i)

        if isnull(u)
            break
        end

        # find new v corresponding to new u
        unode = get(u)
        ukey = unode.keys[i]
        while !isnull(v)
            vnode = get(v)
            vkey = vnode.keys[j]

            if last(vkey) < first(ukey)
                @nextleafkey(vnode, v, j)
            else
                break
            end
        end

        w, k = v, j
    end

    it.u = u
    it.v = v
    it.w = w
    it.i = i
    it.j = j
    it.k = k
    return
end


function successive_nextintersection!{K, V1, B1, V2, B2}(
                        it::IntersectionIterator{K, V1, B1, V2, B2})
    u, w, i, k = it.u, it.w, it.i, it.k

    it.isdone = true

    while true
        if isnull(u)
            return
        end
        unode = get(u)
        ukey = unode.keys[i]

        # find next intersection w
        while !isnull(w)
            wnode = get(w)
            wkey = wnode.keys[k]

            if first(wkey) <= last(ukey)
                if first(ukey) <= last(wkey)
                    it.isdone = false
                    break
                end

                # next w key
                if k < length(wnode)
                    k += 1
                else
                    k = 1
                    w = wnode.right

                    # When we hit a leaf node with no possible intersections, we
                    # start over from the top of the leaf. This is a heuristic
                    # to avoid linear search behavior in the presence of
                    # extremely long intervals.
                    if !isnull(w) && get(w).maxend < first(ukey)
                        wnode = get(w)
                        w, k = firstintersection(it.t2.root, ukey,
                                                 Nullable(wnode.keys[wnode.count]))
                    end
                end
            else
                break
            end
        end

        # intersection was found
        if !it.isdone
            break
        end

        # if no intersection found, advance u and start again
        @nextleafkey(unode, u, i)

        if isnull(u)
            break
        end

        # find u's first intersection
        unode = get(u)
        ukey = unode.keys[i]

        w, k = firstintersection(it.t2.root, ukey, Nullable{Interval{K}}())
    end

    it.u = u
    it.w = w
    it.i = i
    it.k = k
    return
end


function Base.next{K, V1, B1, V2, B2}(it::IntersectionIterator{K, V1, B1, V2, B2}, state)
    if it.successive
        u = get(it.u)
        w = get(it.w)
        value = (u.entries[it.i], w.entries[it.k])
        @nextleafkey(w, it.w, it.k)
        successive_nextintersection!(it)
    else
        u = get(it.u)
        w = get(it.w)
        value = (u.entries[it.i], w.entries[it.k])
        @nextleafkey(w, it.w, it.k)
        iterative_nextintersection!(it)
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
    m = length(t1)
    n = length(t2)

    if method == :auto
        iterative_cost  = n + m
        successive_cost = 0.25 * m * log(1 + n) + 1e5
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

end  # module IntervalTrees
