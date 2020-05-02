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

using Base: notnothing

include("slice.jl")

"""
Types deriving from `AbstractInterval{T}` must have a `first` and `last`
function each returning a value of type `T`, and `first(i) <= last(i)`
must always be true.
"""
abstract type AbstractInterval{T} end

Base.first(i::AbstractInterval{T}) where T = i.first
Base.last(i::AbstractInterval{T}) where T = i.last

function basic_isless(u::AbstractInterval, v::AbstractInterval)
    return first(u) < first(v) || (first(u) == first(v) && last(u) < last(v))
end

function Base.isless(u::AbstractInterval, v::AbstractInterval)
    return basic_isless(u, v)
end

"""
A basic interval.
"""
struct Interval{T} <: AbstractInterval{T}
    first::T
    last::T
end
Base.convert(::Type{Interval{T}}, range::AbstractRange{T}) where T =
    Interval(first(range), last(range))
Interval(range::AbstractRange{T}) where T = convert(Interval{T}, range)

"""
An interval with some associated data.
"""
struct IntervalValue{K, V} <: AbstractInterval{K}
    first::K
    last::K
    value::V
end

IntervalValue(range::AbstractRange{K}, value::V) where {K, V} =
    IntervalValue(first(range), last(range), value)

valtype(::Type{IntervalValue{K,V}}) where {K, V} = V

value(i::IntervalValue{K, V}) where {K, V} = i.value

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

abstract type Node{K, V, B} end


mutable struct InternalNode{K, V, B} <: Node{K, V, B}
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

    parent::Union{Nothing, InternalNode{K, V, B}}

    # Sibling/cousin pointers.
    left::Union{Nothing, InternalNode{K, V, B}}
    right::Union{Nothing, InternalNode{K, V, B}}

    function InternalNode{K,V,B}() where {K,V,B}
        return new{K,V,B}(
            Slice{Interval{K}, B}(), zero(K), Slice{K, B}(), Slice{Node{K, V, B}, B}(),
            nothing,
            nothing,
            nothing)
    end
end


function minstart(t::InternalNode)
    return t.keys[1].first
end


mutable struct LeafNode{K, V, B} <: Node{K, V, B}
    entries::Vector{V}
    keys::Vector{Interval{K}}
    count::UInt32 # number of stored entries

    # maximum interval end-point in this leaf node.
    maxend::K

    parent::Union{Nothing, InternalNode{K, V, B}}

    # Sibling/cousin pointers.
    left::Union{Nothing, LeafNode{K, V, B}}
    right::Union{Nothing, LeafNode{K, V, B}}

    function LeafNode{K,V,B}() where {K,V,B}
        return new{K,V,B}(
            Array{V}(undef, B), Array{Interval{K}}(undef, B), 0,
            zero(K),
            nothing,
            nothing,
            nothing)
    end
end


function minstart(t::LeafNode)
    return first(t.keys[1])
end


function Base.resize!(leaf::LeafNode{K, V, B}, n) where {K, V, B}
    @assert 0 <= n <= B
    leaf.count = n
end


function Base.splice!(leaf::LeafNode{K, V, B}, i::Integer) where {K, V, B}
    slice_splice!(leaf.keys, leaf.count, i)
    x, leaf.count = slice_splice!(leaf.entries, leaf.count, i)
    return x
end


function Base.insert!(leaf::LeafNode{K, V, B}, i::Integer, value::V) where {K, V, B}
    slice_insert!(leaf.keys, leaf.count, i, Interval{K}(first(value), last(value)))
    slice_insert!(leaf.entries, leaf.count, i, value)
    leaf.count += 1
end


function Base.push!(leaf::LeafNode{K, V, B}, value::V) where {K, V, B}
    leaf.count += 1
    leaf.entries[leaf.count] = value
    leaf.keys[leaf.count] = Interval{K}(first(value), last(value))
end


function Base.pop!(leaf::LeafNode{K, V, B}) where {K, V, B}
    x = leaf.entries[leaf.count]
    leaf.count -= 1
    return x
end

function Base.iterate(leaf::LeafNode, i::Int=1)
    return i ≤ length(leaf) ? (leaf.entries[i], i + 1) : nothing
end


mutable struct IntervalBTree{K, V, B}
    root::Node{K, V, B}
    n::Int # Number of entries

    function IntervalBTree{K,V,B}() where {K,V,B}
        return new{K,V,B}(LeafNode{K, V, B}(), 0)
    end

    # Construct an interval tree from a sorted array of intervals.
    #
    # This is generally much more efficient than constructing the same tree by
    # inserting intervals one by one.
    #
    # Args:
    #   entries: Interval entry values in sorted order.
    #
    function IntervalBTree{K,V,B}(entries::AbstractVector{V}) where {K,V,B}
        if !issorted(entries, lt=basic_isless)
            error("Intervals must be sorted to construct an IntervalTree")
        end

        # Here's the plan: figure out how many leaf nodes we need, allocate
        # them all up front. Copy in the keys and values, then work up towards
        # the root.

        n = length(entries)

        if n == 0
            return new{K,V,B}(LeafNode{K, V, B}(), 0)
        end

        numleaves = cld(n, B - 2)
        leaves = [LeafNode{K, V, B}() for _ in 1:numleaves]

        maxends = Vector{K}(undef, numleaves)
        minkeys = Vector{Interval{K}}(undef, numleaves)

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

        return new{K,V,B}(children[1], n)
    end
end


# Default B-tree order
const IntervalTree{K, V} = IntervalBTree{K, V, 64}

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

mutable struct IntervalBTreeIteratorState{K, V, B}
    leaf::Union{Nothing, LeafNode{K, V, B}}
    i::Int
end

function Base.iterate(
        t::IntervalBTree{K, V, B},
        state::IntervalBTreeIteratorState{K, V, B}=iterinitstate(t)) where {K, V, B}
    if state.leaf === nothing || isempty(state.leaf)
        return nothing
    end
    leaf = notnothing(state.leaf)
    entry = leaf.entries[state.i]
    if state.i < length(leaf)
        state.i += 1
    else
        state.leaf = leaf.right
        state.i = 1
    end
    return entry, state
end

function iterinitstate(t::IntervalBTree{K,V,B}) where {K, V, B}
    # traverse to the first leaf node
    node = t.root
    while !isa(node, LeafNode{K, V, B})
        node = node.children[1]
    end
    return IntervalBTreeIteratorState(node, 1)
end


# Iterate from a given starting from the first interval that intersects a given point.
struct IntervalFromIterator{K, V, B}
    t::IntervalBTree{K, V, B}
    p::K
end

Base.eltype(::Type{IntervalFromIterator{K, V, B}}) where {K, V, B} = V
Base.IteratorSize(::Type{IntervalFromIterator{K, V, B}}) where {K, V, B} = Base.SizeUnknown()

function from(t::IntervalBTree{K, V, B}, p) where {K, V, B}
    return IntervalFromIterator{K, V, B}(t, convert(K, p))
end

function Base.iterate(
        it::IntervalFromIterator{K, V, B},
        state::IntervalBTreeIteratorState{K, V, B}=iterinitstate(it)) where {K, V, B}
    return iterate(it.t, state)
end

function iterinitstate(it::IntervalFromIterator{K, V, B}) where {K, V, B}
    node, i = firstfrom(it.t, it.p)
    if i == 0
        return IntervalBTreeIteratorState{K, V, B}(nothing, i)
    else
        return IntervalBTreeIteratorState{K, V, B}(node, i)
    end
end


# Inserting
# ---------


# Split a leaf into two, returning (leftnode, rightnode)
function split!(left::LeafNode{K, V, B}) where {K, V, B}
    right = LeafNode{K, V, B}()
    right.right = left.right
    right.left = left
    left.right = right
    if right.right !== nothing
        right.right = notnothing(right.right)
        right.right.left = right
    end
    right.parent = left.parent

    m = length(left)
    resize!(right, m - div(m, 2))
    copyto!(right.entries, 1, left.entries, div(m, 2) + 1, length(right))
    copyto!(right.keys, 1, left.keys, div(m, 2) + 1, length(right))
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
function split!(left::InternalNode{K, V, B}) where {K, V, B}
    right = InternalNode{K, V, B}()
    right.right = left.right
    right.left = left
    left.right = right
    if right.right !== nothing
        right.right = notnothing(right.right)
        right.right.left = right
    end
    right.parent = left.parent

    m = length(left)

    resize!(right.children, m - div(m, 2))
    resize!(right.maxends, m - div(m, 2))
    resize!(right.keys, m - div(m, 2) - 1)

    copyto!(right.children, 1, left.children, div(m, 2)+1, length(right.children))
    copyto!(right.maxends, 1, left.maxends, div(m, 2)+1, length(right.maxends))
    copyto!(right.keys, 1, left.keys, div(m, 2)+1, length(right.keys))

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
function nodemaxend(t::InternalNode{K, V, B}) where {K, V, B}
    maxend = zero(K)
    for i in 1:length(t.children)
        maxend = max(maxend, t.children[i].maxend)
    end
    return maxend
end


function nodemaxend(t::LeafNode{K, V, B}) where {K, V, B}
    maxend = zero(K)
    for i in 1:length(t)
        maxend = max(maxend, last(t.keys[i]))
    end
    return maxend
end


sameparent(u::Node{K, V, B}, v::Node{K, V, B}) where {K, V, B} = (u.parent == v.parent)

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
    while t.parent !== nothing
        t = notnothing(t.parent)
    end
    return t
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

function minkey(t::LeafNode{K, V, B}) where {K, V, B}
    return t.keys[1]
end


function Base.push!(t::IntervalBTree{K, V, B}, entry::V,
                    unique_key::Bool=false, update::Bool=true) where {K, V, B}
    return _push!(t, t.root, entry, unique_key, update)
end


function _push!(t::IntervalBTree{K, V, B},
                node::InternalNode{K, V, B},
                entry::V, unique_key::Bool, update::Bool) where {K, V, B}
    i = findidx(node, entry) # key index
    j = i <= length(node) - 1 && entry < node.keys[i] ? i : i + 1 # child index
    return _push!(t, node.children[j], entry, unique_key, update)
end


function _push!(t::IntervalBTree{K, V, B},
                node::LeafNode{K, V, B}, entry::V,
                unique_key::Bool, update::Bool) where {K, V, B}
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
        while parent !== nothing
            p = notnothing(parent)
            if hassplit
                i = findidx(p, entry) # key index
                j = i <= length(p) - 1 && entry < p.keys[i] ? i : i + 1 # child index
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
                ifind = findfirst(isequal(child), p.children)
                if ifind === nothing
                    ifind = 0
                end
                p.maxends[ifind] = child.maxend
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
        while parent !== nothing
            p = notnothing(parent)
            p.maxend = max(p.maxend, last(entry))
            ifind = findfirst(isequal(child), p.children)
            if ifind === nothing
                ifind = 0
            end
            p.maxends[ifind] = child.maxend
            parent = p.parent
            child = p
        end
    end

    return entry
end



# Deleting
# --------



function deletefirst!(t::IntervalBTree{K, V, B}, first::K, last::K) where {K, V, B}
    return deletefirst!(t, Interval{K}(first, last))
end


function deletefirst!(t::IntervalBTree{K, V, B}, key::Tuple{K, K}) where {K, V, B}
    return deletefirst!(t, Interval{K}(key[1], key[2]))
end


function deletefirst!(t::IntervalBTree{K, V, B}, key::Interval{K}) where {K, V, B}
    result = _deletefirst!(t.root, key)
    if result.keyfound
        t.n -= 1
    end

    # if the root has only one child, promote the child
    if isa(t.root, InternalNode) && length(t.root) == 1
        t.root = t.root.children[1]
        t.root.parent = nothing
    end

    return t
end


# Indicate what steps are need to account for an updated child.
struct KeyFate
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
struct DeleteResult
    keyfound::Bool # true if the key was found

    # Indicate what steps are need to account for an updated child. One of:
    fate::KeyFate
end


# Delete key from the subtree if present. Return a DeleteResult object.
function _deletefirst!(t::InternalNode{K, V, B}, key::Interval{K}) where {K, V, B}
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
        if t.right !== nothing
            right = notnothing(t.right)
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
        if t.left !== nothing
            left = notnothing(t.left)
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
        if t.left !== nothing
            left = notnothing(t.left)
            if sameparent(left, t)
                merge!(left, t)
                left.right = t.right
                if t.right !== nothing
                    t.right = notnothing(t.right)
                    t.right.left = left
                end

                return DeleteResult(true, KEYFATE_DELETE)
            end
        end

        # merge with right
        if t.right !== nothing
            right = notnothing(t.right)
            if sameparent(right, t)
                merge!(t, right)
                if right.right !== nothing
                    right.right = notnothing(right.right)
                    right.right.left = t
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


function _deletefirst!(t::LeafNode{K, V, B}, key::Interval{K}) where {K, V, B}
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
    if t.right !== nothing
        right = notnothing(t.right)
        if sameparent(right, t) && length(right) > minsize
            push!(t, splice!(right, 1))
            t.maxend = max(t.maxend, last(t.keys[t.count]))
            right.maxend = nodemaxend(right)

            return DeleteResult(true, KEYFATE_UPDATE_RIGHT)
        end
    end

    # borrow left
    if t.left !== nothing
        left = notnothing(t.left)
        if sameparent(left, t) && length(left) > minsize
            insert!(t, 1, pop!(left))
            t.maxend = max(t.maxend, last(t.keys[1]))
            left.maxend = nodemaxend(left)

            return DeleteResult(true, KEYFATE_UPDATE_LEFT)
        end
    end

    # merge with left
    if t.left !== nothing
        left = notnothing(t.left)
        if sameparent(left, t)
            merge!(left, t)
            left.right = t.right
            if t.right !== nothing
                t.right = notnothing(t.right)
                t.right.left = left
            end
            return DeleteResult(true, KEYFATE_DELETE)
        end
    end

    # merge with right
    if t.right !== nothing
        right = notnothing(t.right)
        if sameparent(right, t)
            merge!(t, right)
            if right.right !== nothing
                right.right = notnothing(right.right)
                right.right.left = t
            end
            t.right = right.right
            return DeleteResult(true, KEYFATE_DELETE_RIGHT)
        end
    end

    # This must be the root node. Allow it to be underfull.
    return DeleteResult(true, KEYFATE_NONE)
end


# Join two leaf nodes into one.
function merge!(left::LeafNode{K, V, B}, right::LeafNode{K, V, B}) where {K, V, B}
    leftlen, rightlen = length(left), length(right)
    @assert leftlen + rightlen <= B
    resize!(left, leftlen + rightlen)
    copyto!(left.entries, leftlen+1, right.entries, 1, length(right))
    copyto!(left.keys, leftlen+1, right.keys, 1, length(right))
    left.maxend = nodemaxend(left)
    return
end


# Join two internal nodes into one
function merge!(left::InternalNode{K, V, B}, right::InternalNode{K, V, B}) where {K, V, B}
    leftlen, rightlen = length(left), length(right)
    @assert length(left) + length(right) <= B
    resize!(left.keys, leftlen + rightlen - 1)
    resize!(left.children, leftlen + rightlen)
    resize!(left.maxends, leftlen + rightlen)
    copyto!(left.children, leftlen+1, right.children, 1, length(right.children))
    copyto!(left.maxends, leftlen+1, right.maxends, 1, length(right.maxends))
    for i in leftlen+1:length(left.children)
        left.children[i].parent = left
    end
    left.keys[leftlen] = minkey(left.children[leftlen+1])
    copyto!(left.keys, leftlen+1, right.keys, 1, length(right.keys))
    left.maxend = nodemaxend(left)
    return
end


# Searching
# ---------

function findidx(t::LeafNode{K, V, B}, key::AbstractInterval{K}) where {K, V, B}
    if isempty(t)
        return 0
    end
    # TODO: search keys
    return searchsortedfirst(t.entries, key, 1, Int(t.count),
                             Base.ord(basic_isless, identity, false))
end

function findidx(t::InternalNode{K, V, B}, key::AbstractInterval{K}) where {K, V, B}
    if isempty(t.keys)
        return 0
    end
    i = searchsortedfirst(t.keys, key)
    return min(i, length(t.keys))
end

function Base.haskey(t::LeafNode{K, V, B}, key::AbstractInterval{K}) where {K, V, B}
    i = findidx(t, key)
    return 1 <= i <= length(t) && first(t.keys[i]) == first(key) &&
           last(t.keys[i]) == last(key)
end


function Base.haskey(t::InternalNode{K, V, B}, key::AbstractInterval{K}) where {K, V, B}
    i = findidx(t, key)
    if i <= length(t) - 1 && !(key < t.keys[i])
        return haskey(t.children[i+1], key)
    else
        return haskey(t.children[i], key)
    end
end


function Base.haskey(t::IntervalBTree{K, V, B}, key0::Tuple{Any, Any}) where {K, V, B}
    key = Interval{K}(key0[1], key0[2])
    return haskey(t.root, key)
end


function Base.findfirst(t::LeafNode{K, V, B}, key::AbstractInterval{K}, f) where {K, V, B}
    i = findidx(t, key)
    while 1 <= i <= length(t) &&
          first(t.keys[i]) == first(key) &&
          last(t.keys[i]) == last(key)
        if f(t.entries[i], key)
            return t.entries[i]
        end
        i += 1
        if i > length(t) && t.right !== nothing
            t = notnothing(t.right)
            i = 1
        end
    end

    return nothing
end


function Base.findfirst(t::InternalNode{K, V, B}, key::AbstractInterval{K}, f) where {K, V, B}
    i = findidx(t, key)
    if i <= length(t) - 1 && !(key < t.keys[i])
        return findfirst(t.children[i+1], key, f)
    else
        return findfirst(t.children[i], key, f)
    end
end


true_cmp(a, b) = true

function Base.findfirst(t::IntervalBTree{K, V, B}, key::AbstractInterval{K},
                        f=true_cmp) where {K, V, B}
    return findfirst(t.root, key, f)
end


function Base.findfirst(t::IntervalBTree{K, V, B}, key0::Tuple{Any, Any},
                        f=true_cmp) where {K, V, B}
    return findfirst(t, Interval{K}(key0[1], key0[2]), f)
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
@inline function intersects(key1::AbstractInterval{K}, key2::AbstractInterval{K}) where K
    return first(key1) <= last(key2) && first(key2) <= last(key1)
end


# Return true if the tree has an intersection with the given position
function hasintersection(t::IntervalBTree{K, V, B}, query) where {K, V, B}
    return hasintersection(t.root, convert(K, query))
end


function hasintersection(t::InternalNode{K, V, B}, query::K) where {K, V, B}
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


function hasintersection(t::LeafNode{K, V, B}, query::K) where {K, V, B}
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
mutable struct Intersection{K, V, B}
    index::Int
    node::LeafNode{K, V, B}

    Intersection{K,V,B}(index, node) where {K,V,B} = new{K,V,B}(index, node)
    Intersection{K,V,B}() where {K,V,B} = new{K,V,B}(0)
end


# Find the first interval in the tree that intersects the query and return
# as a (leafnode, index) pair, indicating that leafnode.keys[index] intersects.
# If no intersection is found, index is 0 and leafnode is the last node
# searched.
function firstintersection!(t::IntervalBTree{K, V, B},
                            query::AbstractInterval{K},
                            lower::Union{Nothing, V},
                            out::Intersection{K, V, B},
                            filter::F) where {F, K, V, B}
    return firstintersection!(t.root, query, lower, out, filter)
end


function firstintersection!(t::InternalNode{K, V, B},
                            query::AbstractInterval{K},
                            lower::Union{Nothing, V},
                            out::Intersection{K, V, B},
                            filter::F) where {F, K, V, B}
    if isempty(t) || t.maxend < first(query)
        out.index = 0
        return
    end

    (query_first, query_last) = (first(query), last(query))

    i = lower === nothing ? 1 : searchsortedfirst(t.keys, notnothing(lower))

    @inbounds while i <= length(t.children)
        if i > 1 && t.keys[i-1].first > query_last
            break
        end
        if t.maxends[i] >= query_first
            firstintersection!(t.children[i], query, lower, out, filter)
            if out.index > 0
                return
            end
        end
        i += 1
    end

    out.index = 0
    return
end


function firstintersection!(t::LeafNode{K, V, B},
                            query::AbstractInterval{K},
                            lower::Union{Nothing, V},
                            out::Intersection{K, V, B},
                            filter::F) where {F, K, V, B}
    if isempty(t) || t.maxend < first(query)
        out.index = 0
        return
    end

    # TODO: search keys
    i = lower === nothing ? 1 :
        searchsortedfirst(t.entries, notnothing(lower), 1, Int(t.count),
                          Base.ord(basic_isless, identity, false))

    while i <= length(t)
        if intersects(t.entries[i], query) && filter(t.entries[i], query)
            out.index = i
            out.node = t
            return
        elseif last(query) < first(t.keys[i])
            break
        end
        i += 1
    end

    out.index = 0
    return
end


function firstintersection(t::InternalNode{K, V, B},
                           query::Interval{K},
                           lower::Union{Nothing, Interval{K}}) where {K, V, B}
    if isempty(t) || t.maxend < first(query)
        return (nothing, 1)
    end

    i = lower === nothing ? 1 : searchsortedfirst(t.keys, notnothing(lower))

    @inbounds while i <= length(t.children)
        if i > 1 && t.keys[i-1].first > last(query)
            break
        end
        if t.maxends[i] >= first(query)
            w, k = firstintersection(t.children[i], query, lower)
            w !== nothing && return notnothing(w), k
        end
        i += 1
    end

    return (nothing, 1)
end


function firstintersection(t::LeafNode{K, V, B}, query::Interval{K},
                           lower::Union{Nothing, Interval{K}}) where {K, V, B}

    if isempty(t) || t.maxend < first(query)
        return (nothing, 1)
    end

    i = lower === nothing ? 1 : searchsortedfirst(t.keys, notnothing(lower))

    while i <= length(t)
        if intersects(t.keys[i], query) &&
           (lower === nothing || !basic_isless(t.keys[i], notnothing(lower)))
            return (t, i)
        elseif last(query) < first(t.keys[i])
            break
        end
        i += 1
    end

    return (nothing, 1)
end


# Find the first key with an end point >= query
function firstfrom(t::IntervalBTree{K, V, B}, query::K) where {K, V, B}
    return firstfrom(t.root, query)
end


function firstfrom(t::InternalNode{K, V, B}, query::K) where {K, V, B}
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


function firstfrom(t::LeafNode{K, V, B}, query::K) where {K, V, B}
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
function nextintersection!(t::LeafNode{K, V, B}, i::Integer,
                           query::AbstractInterval{K},
                           out::Intersection{K, V, B},
                           filter::F) where {F, K, V, B}
    j = i + 1
    while true
        while j <= length(t)
            if intersects(t.keys[j], query) && filter(t.entries[j], query)
                out.index = j
                out.node = t
                return
            end
            j += 1
        end
        j = 1

        t.right === nothing && break

        t = notnothing(t.right)

        if minstart(t) > last(query)
            break
        end

        # When we hit a leaf node with no possible intersections, we start over
        # from the top of the leaf. This is a heuristic to avoid linear search
        # behavior in the presence of extremely long intervals.
        if t.maxend < first(query)
            return firstintersection!(root(t), query,
                                      t.entries[t.count], out, filter)
        end
    end

    out.index = 0
    return
end


mutable struct IntervalIntersectionIterator{F, K, V, B}
    filter::F
    intersection::Intersection{K, V, B}
    t::IntervalBTree{K, V, B}
    query::AbstractInterval{K}

    function IntervalIntersectionIterator{F,K,V,B}(filter, intersection, t, query) where {F,K,V,B}
        return new{F,K,V,B}(filter, intersection, t, query)
    end

    function IntervalIntersectionIterator{F,K,V,B}() where {F,K,V,B}
        return new{F,K,V,B}(Intersection{F, K, V, B}())
    end
end

Base.eltype(::Type{IntervalIntersectionIterator{F,K,V,B}}) where {F,K,V,B} = V
Base.IteratorSize(::Type{IntervalIntersectionIterator{F,K,V,B}}) where {F,K,V,B} = Base.SizeUnknown()

# Intersect an interval tree t with a single interval, returning an iterator
# over the intersecting (key, value) pairs in t.
function Base.intersect(t::IntervalBTree{K, V, B}, query0::Tuple{Any, Any},
                        filter::F=true_cmp) where {F, K, V, B}
    query = Interval{K}(query0[1], query0[2])
    return intersect(t, query, filter)
end


function Base.intersect(t::IntervalBTree{K, V, B}, first::K, last::K,
                        filter::F=true_cmp) where {F, K, V, B}
    query = Interval{K}(first, last)
    return intersect(t, query, filter)
end


function Base.intersect(t::IntervalBTree{K, V, B}, query::AbstractInterval{K},
                        filter::F=true_cmp) where {F, K, V, B}
    return IntervalIntersectionIterator{F, K, V, B}(filter, Intersection{K, V, B}(), t, query)
end

function Base.iterate(it::IntervalIntersectionIterator{F, K, V, B}) where {F, K, V, B}
    firstintersection!(it.t, it.query, nothing, it.intersection, it.filter)
    return iterate(it, nothing)
end

function Base.iterate(it::IntervalIntersectionIterator{F, K, V, B}, _) where {F, K, V, B}
    intersection = it.intersection
    if intersection.index == 0
        return nothing
    end
    entry = intersection.node.entries[intersection.index]
    nextintersection!(intersection.node, intersection.index,
                      it.query, intersection, it.filter)
    return entry, nothing
end


mutable struct IntersectionIterator{F, K, V1, B1, V2, B2}
    filter::F

    t1::IntervalBTree{K, V1, B1}
    t2::IntervalBTree{K, V2, B2}

    # if true, use successive intersection, if false iterative.
    successive::Bool

    # true if done
    isdone::Bool

    # intersection state
    u::Union{Nothing, LeafNode{K, V1, B1}}
    v::Union{Nothing, LeafNode{K, V2, B2}}
    w::Union{Nothing, LeafNode{K, V2, B2}}
    i::Int
    j::Int
    k::Int

    function IntersectionIterator{F,K,V1,B1,V2,B2}(filter::F, t1, t2, successive::Bool) where {F,K,V1,B1,V2,B2}
        return new{F,K,V1,B1,V2,B2}(filter, t1, t2, successive)
    end
end

Base.eltype(::Type{IntersectionIterator{F,K,V1,B1,V2,B2}}) where {F,K,V1,B1,V2,B2} = Tuple{V1,V2}
Base.IteratorSize(::Type{IntersectionIterator{F,K,V1,B1,V2,B2}}) where {F,K,V1,B1,V2,B2} = Base.SizeUnknown()

function Base.iterate(it::IntersectionIterator, _=iterinitstate(it))
    if it.isdone
        return nothing
    end
    if it.successive
        u = it.u
        w = it.w
        value = (u.entries[it.i], w.entries[it.k])
        @nextleafkey(w, it.w, it.k)
        successive_nextintersection!(it)
    else
        u = it.u
        w = it.w
        value = (u.entries[it.i], w.entries[it.k])
        @nextleafkey(w, it.w, it.k)
        iterative_nextintersection!(it)
    end
    return value, nothing
end

function iterinitstate(it::IntersectionIterator)
    it.isdone = true
    if it.successive
        it.u = isempty(it.t1) ? nothing : firstleaf(it.t1)
        it.w = isempty(it.t2) ? nothing : firstleaf(it.t2)
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

        it.u = isempty(it.t1) ? nothing : firstleaf(it.t1)
        it.v = isempty(it.t2) ? nothing : firstleaf(it.t2)
        it.w = it.v
        it.i, it.j, it.k = 1, 1, 1
        iterative_nextintersection!(it)
    end
    return nothing  # iterator is stateful
end

function iterative_nextintersection!(
    it::IntersectionIterator{F, K, V1, B1, V2, B2}) where {F, K, V1, B1, V2, B2}

    u, v, w, i, j, k = it.u, it.v, it.w, it.i, it.j, it.k

    it.isdone = true

    while true
        u === nothing && return
        unode = notnothing(u)
        ukey = unode.keys[i]

        v === nothing && return
        vnode = notnothing(v)
        vkey = vnode.keys[j]

        # find next intersection w
        while w !== nothing
            wnode = notnothing(w)
            wkey = wnode.keys[k]

            if first(wkey) <= last(ukey)
                if first(ukey) <= last(wkey) && it.filter(unode.entries[i], wnode.entries[k])
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

        u === nothing && break

        # find new v corresponding to new u
        unode = notnothing(u)
        ukey = unode.keys[i]
        while v !== nothing
            vnode = notnothing(v)
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


function successive_nextintersection!(
    it::IntersectionIterator{F, K, V1, B1, V2, B2}) where {F, K, V1, B1, V2, B2}

    u, w, i, k = it.u, it.w, it.i, it.k

    it.isdone = true

    while true
        u === nothing && return
        unode = notnothing(u)
        ukey = unode.keys[i]

        # find next intersection w
        while w !== nothing
            wnode = notnothing(w)
            wkey = wnode.keys[k]

            if first(wkey) <= last(ukey)
                if first(ukey) <= last(wkey) && it.filter(unode.entries[i], wnode.entries[k])
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
                    if w !== nothing && w.maxend < first(ukey)
                        wnode = notnothing(w)
                        w, k = firstintersection(it.t2.root, ukey,
                                                 wnode.keys[wnode.count])
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

        u === nothing && break

        # find u's first intersection
        unode = notnothing(u)
        ukey = unode.keys[i]

        w, k = firstintersection(it.t2.root, ukey, nothing)
    end

    it.u = u
    it.w = w
    it.i = i
    it.k = k
    return
end



# Intersect two interval trees, returning an iterator yielding values of the
# form:
#   ((key1, value1), (key2, value2))
#
# Where key1 is from the first tree and key2 from the second, and they
# intersect.
function Base.intersect(t1::IntervalBTree{K, V1, B1},
                        t2::IntervalBTree{K, V2, B2},
                        filter::F=true_cmp; method=:auto) where {F, K, V1, B1, V2, B2}
    # We decide heuristically which intersection algorithm to use.
    m = length(t1)
    n = length(t2)

    if method == :auto
        iterative_cost  = n + m
        successive_cost = 0.25 * m * log(1 + n) + 1e5
        if iterative_cost < successive_cost
            return IntersectionIterator{F, K, V1, B1, V2, B2}(filter, t1, t2, false)
        else
            return IntersectionIterator{F, K, V1, B1, V2, B2}(filter, t1, t2, true)
        end
    elseif method == :successive
        return IntersectionIterator{F, K, V1, B1, V2, B2}(filter, t1, t2, true)
    elseif method == :iterative
        return IntersectionIterator{F, K, V1, B1, V2, B2}(filter, t1, t2, false)
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
