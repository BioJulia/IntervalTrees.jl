#!/usr/bin/env julia

if VERSION >= v"0.5-"
    using Base.Test
else
    using BaseTestNext
    const Test = BaseTestNext
end

using IntervalTrees
import IntervalTrees: Slice, InternalNode, LeafNode, Interval, IntervalBTree


# Getters
@testset "Getters" begin
    i = Interval(5, 6)
    iv = IntervalValue(10, 11, "FOO")
    @test first(i)  === 5
    @test last(i)   === 6
    @test first(iv) === 10
    @test last(iv)  === 11
    @test value(iv) == "FOO"
end

# Generating random intervals
srand(12345)
const maxend = round(Int, 1e9)

function randinterval(minstart=1, maxend=maxend)
    a = rand(minstart:maxend)
    b = rand(a:maxend)
    return (a, b)
end


# Verify that internal node keys and maxend values are correct
function validkeys{K, V, B}(t::IntervalTrees.IntervalBTree{K, V, B})
    minint = IntervalTrees.Interval{K}(0, 0)
    maxint = IntervalTrees.Interval{K}(maxend, maxend)
    return validkeys(t.root, minint, maxint)
end


function validkeys(node::IntervalTrees.InternalNode, minint, maxint)
    for child in node.children
        if child.maxend > node.maxend
            return false
        end
    end

    for i in 1:length(node.keys)
        k = node.keys[i]
        if IntervalTrees.minkey(node.children[i+1]) != k
            return false
        end

        if !validkeys(node.children[i], minint, k) ||
           !validkeys(node.children[i+1], k, maxint)
           return false
       end
    end
    return true
end


function validkeys(node::IntervalTrees.LeafNode, minint, maxint)
    for entry in node.entries
        if !(minint <= entry <= maxint) || last(entry) > node.maxend
            return false
        end
    end
    return true
end


# Verify that parent pointers are correct
function validparents(t::IntervalTrees.IntervalBTree)
    if !isnull(t.root.parent)
        return false
    end

    return validparents(t.root)
end


function validparents(node::IntervalTrees.InternalNode)
    for child in node.children
        if !validparents(child) || get(child.parent) != node
            return false
        end
    end
    return true
end

function validparents(node::IntervalTrees.LeafNode)
    return true
end


# Verify that sibling/cousin pointers are correct
function validsiblings(t::IntervalTrees.IntervalBTree)
    if !isnull(t.root.left) || !isnull(t.root.right)
        return false
    end

    # Do an in-order traversal, pushing nodes onto a stack indexed by
    # depth.
    tdepth = depth(t)
    nodestacks = [Array(IntervalTrees.Node, 0) for _ in 1:tdepth]

    function _visit(node::IntervalTrees.InternalNode, k)
        push!(nodestacks[k], node)
        for child in node.children
            _visit(child, k+1)
        end
    end

    function _visit(node::IntervalTrees.LeafNode, k)
        push!(nodestacks[k], node)
    end

    _visit(t.root, 1)

    for k in 1:tdepth
        stack = nodestacks[k]
        if isempty(stack)
            continue
        end

        if !isnull(stack[1].left) || !isnull(stack[end].right)
            return false
        end

        if length(stack) > 1
            if isnull(stack[1].right) || get(stack[1].right) != stack[2] ||
               isnull(stack[end].left) || get(stack[end].left) != stack[end-1]
                return false
            end
        end

        for i in 2:length(stack)-1
            if isnull(stack[i].left) || get(stack[i].left) != stack[i-1] ||
               isnull(stack[i].right) || get(stack[i].right) != stack[i+1]
                return false
            end
        end
    end

    return true
end


@testset "Search" begin
    t = IntervalMap{Int, Int}()
    @test !haskey(t, (1,2))

    n = 10000
    maxend = 1000000
    intervals = [randinterval(1, maxend) for i in 1:n]

    @testset "true positives" begin
        for (i, interval) in enumerate(intervals)
            t[interval] = i
        end

        shuffle!(intervals)
        @test all(Bool[haskey(t, interval) for interval in intervals])
        @test all(Bool[get!(t, interval, -1) != IntervalValue{Int, Int}(interval[1], interval[2], -1)
                       for interval in intervals])
        @test all(Bool[get(t, interval, -1) != -1 for interval in intervals])
    end

    @testset "true negatives" begin
        @test all(Bool[!haskey(t, interval)
                       for interval in [randinterval(maxend+1, 2 * maxend)
                                        for i in 1:n]])
        @test all(Bool[get!(t, interval, -1) == IntervalValue{Int, Int}(interval[1], interval[2], -1)
                       for interval in [randinterval(maxend+1, 2 * maxend)
                                        for i in 1:n]])
        @test all(Bool[get(t, interval, -1) == -1
                       for interval in [randinterval(maxend+1, 2 * maxend)
                                        for i in 1:n]])
    end
end


@testset "Iteration" begin
    t = IntervalMap{Int, Int}()
    @test isempty([x for x in t])

    @testset "from" begin
        n = 100
        maxend = 1000000
        intervals = [randinterval(1, maxend) for _ in 1:n]
        startpos = 50000
        expected_count = 0
        t = IntervalMap{Int, Int}()
        for (i, interval) in enumerate(intervals)
            t[interval] = i
            if interval[2] >= startpos
                expected_count += 1
            end
        end

        @test length(collect(from(t, startpos))) === expected_count
        @test length(collect(from(t, 0))) === n
        @test length(collect(from(t, maxend + 1))) === 0
        @test length(collect(from(IntervalTree{Int, Int}(), 0))) === 0
    end
end


@testset "Interval Intersection" begin
    # generate n end-to-end intervals
    t = IntervalMap{Int, Int}()
    intervals = Any[]
    a = 1
    for i in 1:10000
        b = a + rand(0:100)
        t[(a, b)] = i
        push!(intervals, (a, b))
        a = b + 1
    end

    # one
    x = rand(0:a-1)
    @test length(collect(intersect(t, (x, x)))) === 1
    @test hasintersection(t, x)

    # nothing
    @test length(collect(intersect(t, (a, a)))) === 0
    @test !hasintersection(t, a)

    # everything
    @test length(collect(intersect(t, (intervals[1][1], intervals[end][2])))) == length(t)

    @test all(Bool[hasintersection(t, interval[1]) for interval in intervals])
    @test all(Bool[hasintersection(t, interval[end]) for interval in intervals])

    # some random intersection queries
    function random_intersection_query()
        i = rand(1:length(intervals))
        j = rand(i:length(intervals))
        return length(collect(intersect(t, (intervals[i][1], intervals[j][2])))) == j - i + 1
    end

    @test all(Bool[random_intersection_query() for _ in 1:1000])

    # intervals separated by 1
    t = IntervalMap{Int, Int}()

    @test !hasintersection(t, 1)

    intervals = Any[]
    gaps = Any[]
    a = 1
    for i in 1:10000
        b = a + rand(0:100)
        if iseven(b)
            t[a, b] = i
            push!(intervals, (a, b))
        else
            push!(gaps, (a, b))
        end
        a = b + 1
    end

    @test all(Bool[hasintersection(t, interval[1]) for interval in intervals])
    @test all(Bool[hasintersection(t, interval[end]) for interval in intervals])
    @test !all(Bool[hasintersection(t, interval[1]) for interval in gaps])
    @test !all(Bool[hasintersection(t, interval[end]) for interval in gaps])
end


@testset "Nonunique" begin
    t = IntervalTree{Int, IntervalValue{Int, Int}}()
    n = 1000
    for i in 1:n
        push!(t, IntervalValue{Int, Int}(50, 100, i))
    end
    push!(t, IntervalValue{Int, Int}(1, 200, n + 1))
    push!(t, IntervalValue{Int, Int}(1, 1, n + 2))
    push!(t, IntervalValue{Int, Int}(200, 200, n + 3))

    @test (length(collect(t)) == length(t) == n + 3)
    @test issorted(collect(keys(t)))
    @test validkeys(t)
    @test validparents(t)
    @test validsiblings(t)
end


@testset "Tree Intersection" begin
    n = 10000
    t1 = IntervalTrees.IntervalMap{Int, Int}()
    t2 = IntervalTrees.IntervalMap{Int, Int}()
    maxend = 1000000
    for k in 1:n
        # generate small-ish intervals so we avoid the worst case
        # of O(n^2) intersecting pairs and this test runs quickly
        u = rand(1:maxend)
        v = rand(u:u+1000)
        t1[(u,v)] = k

        u = rand(1:maxend)
        v = rand(u:u+1000)
        t2[(u,v)] = k
    end

    # Since IntervalTrees has two tree intersection algorithms, I'm
    # testing by checking that they are in agreement.
    a = intersect(t1, t2, method=:successive)
    b = intersect(t1, t2, method=:iterative)
    @test collect(a) == collect(b)

    ## handle a particular intersection case that may not otherwise get hit
    t1 = IntervalMap{Int, Int}()
    t1[(1, 2)] = 1
    t2 = IntervalMap{Int, Int}()
    t2[(1001, 1002)] = 2
    @test isempty(collect(intersect(t1, t2, method=:iterative)))
    @test isempty(collect(intersect(t1, t2, method=:successive)))
end


@testset "Insertion" begin
    t = IntervalMap{Int, Int}()
    n = 100000

    @testset "random insertions" begin
        for v in 1:n
            k = randinterval()
            t[k] = v
        end
        @test issorted(collect(keys(t)))
        @test validkeys(t)
        @test validparents(t)
        @test validsiblings(t)
    end

    @testset "ordered insertions" begin
        for v in 1:n
            k = (v,rand(v:maxend))
            t[k] = v
        end
        @test issorted(collect(keys(t)))
        @test validkeys(t)
        @test validparents(t)
        @test validsiblings(t)
    end

    @testset "reverse ordered insertions" begin
        for v in 1:n
            k = (maxend-v,rand((maxend-v):maxend))
            t[k] = v
        end
        @test issorted(collect(keys(t)))
        @test validkeys(t)
        @test validparents(t)
        @test validsiblings(t)
    end

    @testset "bulk insertion" begin
        intervals = IntervalValue{Int, Int}[]
        for v in 1:n
            a, b = randinterval()
            push!(intervals, IntervalValue{Int, Int}(a, b, v))
        end
        sort!(intervals)
        t = IntervalMap{Int, Int}(intervals)

        @test collect(keys(t)) == [Interval{Int}(interval.first, interval.last)
                                   for interval in intervals]
        @test collect(values(t)) == [interval.value for interval in intervals]
        @test issorted(collect(keys(t)))
        @test validkeys(t)
        @test validparents(t)
        @test validsiblings(t)

        # don't break on empty arrays
        t = IntervalMap{Int, Int}(IntervalValue{Int, Int}[])
        @test issorted(collect(keys(t)))
        @test validkeys(t)
        @test validparents(t)
        @test validsiblings(t)

        @test_throws MethodError IntervalTree{Int, Int}(intervals, Int[1])
        shuffle!(intervals)
        @test_throws UndefVarError IntervalTree{Int, Int}(intervals, vals)
    end
end


@testset "Updates" begin
    t = IntervalMap{Int, Int}()
    n = 100000
    ks = [randinterval() for _ in 1:n]
    for (v, k) in enumerate(ks)
        t[k] = v
    end

    shuffle!(ks)
    for (v, k) in enumerate(ks)
        t[k] = v
    end

    @test issorted(collect(keys(t)))
    @test validkeys(t)
    @test validparents(t)
    @test validsiblings(t)
end


@testset "Deletion" begin
    # Set B to smaller number to try to induce more weird merges and borrows and
    # such
    t = IntervalBTree{Int, IntervalValue{Int, Int}, 8}()
    n = 100000

    # insert n random intervals
    intervals = [randinterval() for _ in 1:n]
    for (i, interval) in enumerate(intervals)
        t[interval] = i
    end
    shuffle!(intervals)

    # delete non-existant is a nop
    @test delete!(t, (-1, -1)).n === n

    # delete one
    interval = pop!(intervals)
    delete!(t, interval)
    @test validkeys(t)
    @test validparents(t)
    @test validsiblings(t)
    @test issorted(collect(keys(t)))
    @test !haskey(t, interval)
    @test all(interval -> haskey(t, interval), intervals)

    # delete a random 50%
    for interval in 1:(n-1)/2
        interval = pop!(intervals)
        delete!(t, interval)
    end
    @test validkeys(t)
    @test validparents(t)
    @test validsiblings(t)
    @test issorted(collect(keys(t)))
    @test all(interval -> haskey(t, interval), intervals)

    # delete the rest
    for interval in intervals
        delete!(t, interval)
    end
    @test isempty(t)
    @test validkeys(t)
    @test validparents(t)
    @test validsiblings(t)

    # delete left-to-right
    shuffle!(intervals)
    for (i, interval) in enumerate(intervals)
        t[interval] = i
    end
    sort!(intervals)
    for interval in intervals
        delete!(t, interval)
    end
    @test isempty(t)
    @test validkeys(t)
    @test validparents(t)
    @test validsiblings(t)

    # delete right-to-left
    shuffle!(intervals)
    for (i, interval) in enumerate(intervals)
        t[interval] = i
    end
    reverse!(intervals)
    for interval in intervals
        delete!(t, interval)
    end
    @test isempty(t)
    @test validkeys(t)
    @test validparents(t)
    @test validsiblings(t)

    # delete from the middle
    shuffle!(intervals)
    for (i, interval) in enumerate(intervals)
        t[interval] = i
    end
    mid = div(length(intervals), 2)
    for i in mid+1:length(intervals)
        delete!(t, intervals[i])
    end
    for i in mid:-1:1
        delete!(t, intervals[i])
    end
    @test isempty(t)
    @test validkeys(t)
    @test validparents(t)
    @test validsiblings(t)

    # contrive a special case: merge with the left when we have a non-null
    # right
    t = IntervalBTree{Int, IntervalValue{Int, Int}, 4}()
    for i in 1:24
        t[(i, i)] = i
    end
    @test isa(t.root, InternalNode)
    @test length(t.root.children) === 3
    @test length(t.root.children[2].children) === 2
    delete!(t, (15, 15))
    keys_to_delete = collect(keys(t))
    for key in keys_to_delete
        delete!(t, key)
    end
    @test isempty(t)
    @test validkeys(t)
    @test validparents(t)
    @test validsiblings(t)
end


@testset "Slices" begin
    xs = Slice{Int, 10}()

    @test_throws BoundsError xs[0]
    @test_throws BoundsError xs[11]
    @test_throws BoundsError xs[0] = 1
    @test_throws BoundsError xs[11] = 1
    @test_throws BoundsError pop!(xs)
    @test_throws BoundsError insert!(xs, 0, 1)

    for x in 1:10
        push!(xs, x)
    end

    @test_throws BoundsError push!(xs, 11)
    @test_throws BoundsError insert!(xs, 11, 1)
    @test_throws BoundsError resize!(xs, 11)
    @test_throws BoundsError splice!(xs, 0)
    @test_throws BoundsError splice!(xs, 11)
end


# Abuse low-level functions to test cases that wouldn't otherwise occur
@testset "Low Level" begin
    # findidx should return 0 when called on an empty node
    K = Int
    V = IntervalValue{Int}
    B = 32
    x = Interval{Int}(1, 1)
    node = InternalNode{K, V, B}()
    @test IntervalTrees.findidx(node, x) === 0
    @test IntervalTrees.firstfrom(node, 1) == (node, 0)
    node = LeafNode{K, V, B}()
    @test IntervalTrees.findidx(node, x) == 0

    result = IntervalTrees.Intersection{K, V, B}()
    IntervalTrees.firstintersection!(node, x, result)
    @test result.index == 0

    @test IntervalTrees.firstfrom(node, 1) == (node, 0)

    push!(node.entries, IntervalValue{Int, Int}(1, 1, 1))
    @test IntervalTrees.firstfrom(node, 2) == (node, 0)

    # test that delete! still works on a contrived tree with one internal and
    # one leaf node
    t = IntervalBTree{K, V, B}()
    t.root = InternalNode{K, V, B}()
    push!(t.root.children, LeafNode{K, V, B}())
    push!(t.root.keys, x)
    push!(t.root.children[1].entries, IntervalValue{Int, Int}(1, 1, 1))
    delete!(t, (1,1))
    @test isa(t.root, LeafNode)

    ## test that the right thing happens if you delete the last key in a non-root
    ## leaf node (which can't actually happen)
    node = LeafNode{Int, IntervalValue{Int}, 32}()
    push!(node.entries, IntervalValue{Int, Int}(1, 1, 1))
    @test IntervalTrees.findidx(node, x) == 1
end
