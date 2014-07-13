#!/usr/bin/env julia

module TestIntervalTrees

using FactCheck
using IntervalTrees
import IntervalTrees: Slice, InternalNode, LeafNode, Interval, IntervalBTree


# Generating random intervals
srand(12345)
const maxend = int(1e9)

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
    for k in node.keys
        if !(minint <= k < maxint) || k.b > node.maxend
            return false
        end
    end
    return true
end


# Verify that parent pointers are correct
function validparents(t::IntervalTrees.IntervalBTree)
    if !isa(t.root.parent, IntervalTrees.NullNode)
        return false
    end

    return validparents(t.root)
end


function validparents(node::IntervalTrees.InternalNode)
    for child in node.children
        if !validparents(child) || child.parent != node
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
    if !isa(t.root.left, IntervalTrees.NullNode) ||
       !isa(t.root.right, IntervalTrees.NullNode)
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

       if !isa(stack[1].left, IntervalTrees.NullNode) ||
          !isa(stack[end].right, IntervalTrees.NullNode)
           return false
       end

       if length(stack) > 1
           if stack[1].right != stack[2] || stack[end].left != stack[end-1]
               return false
           end
       end

       for i in 2:length(stack)-1
           if stack[i].left != stack[i-1] || stack[i].right != stack[i+1]
               return false
           end
       end
   end

   return true
end


facts("Search") do
    t = IntervalTree{Int, Int}()
    @fact haskey(t, (1,2)) => false

    n = 10000
    maxend = 1000000
    intervals = [randinterval(1, maxend) for i in 1:n]

    context("true positives") do
        for (i, interval) in enumerate(intervals)
            t[interval] = i
        end

        shuffle!(intervals)
        @fact all([haskey(t, interval) for interval in intervals]) => true
        @fact all([get!(t, interval, -1) != -1 for interval in intervals]) => true
    end

    context("true negatives") do
        @fact all([!haskey(t, interval)
                   for interval in [randinterval(maxend+1, 2 * maxend)
                                    for i in 1:n]]) => true
        @fact all([get!(t, interval, -1) == -1
                   for interval in [randinterval(maxend+1, 2 * maxend)
                                    for i in 1:n]]) => true
    end
end


facts("Iteration") do
    t = IntervalTree{Int, Int}()

    @fact isempty([x for x in t]) => true
end


facts("Interval Intersection") do
    # generate n end-to-end intervals
    t = IntervalTree{Int, Int}()
    intervals = {}
    a = 1
    for i in 1:10000
        b = a + rand(0:100)
        t[(a, b)] = i
        push!(intervals, (a, b))
        a = b + 1
    end

    # one
    x = rand(0:a-1)
    @fact length(collect(intersect(t, (x, x)))) => 1
    @fact hasintersection(t, x) => true

    # nothing
    @fact length(collect(intersect(t, (a, a)))) => 0
    @fact hasintersection(t, a) => false

    # everything
    @fact length(collect(intersect(t, (intervals[1][1], intervals[end][2])))) => length(t)

    @fact all([hasintersection(t, interval[1]) for interval in intervals]) => true
    @fact all([hasintersection(t, interval[end]) for interval in intervals]) => true

    # some random intersection queries
    function random_intersection_query()
        i = rand(1:length(intervals))
        j = rand(i:length(intervals))
        return length(collect(intersect(t, (intervals[i][1], intervals[j][2])))) == j - i + 1
    end

    @fact all([random_intersection_query() for _ in 1:1000]) => true

    # intervals separated by 1
    t = IntervalTree{Int, Int}()

    @fact hasintersection(t, 1) => false

    intervals = {}
    gaps = {}
    a = 1
    for i in 1:10000
        b = a + rand(0:100)
        if iseven(b)
            t[(a, b)] = i
            push!(intervals, (a, b))
        else
            push!(gaps, (a, b))
        end
        a = b + 1
    end

    @fact all([hasintersection(t, interval[1]) for interval in intervals]) => true
    @fact all([hasintersection(t, interval[end]) for interval in intervals]) => true
    @fact all([hasintersection(t, interval[1]) for interval in gaps]) => false
    @fact all([hasintersection(t, interval[end]) for interval in gaps]) => false
end


facts("Tree Intersection") do
    n = 10000
    t1 = IntervalTrees.IntervalTree{Int, Int}()
    t2 = IntervalTrees.IntervalTree{Int, Int}()
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
    a = IntervalTrees.SuccessiveTreeIntersectionIterator(t1, t2)
    b = IntervalTrees.IterativeTreeIntersectionIterator(t1, t2)
    @fact collect(a) == collect(b) => true

    # handle a particular intersection case that may not otherwise get hit
    t1 = IntervalTree{Int, Int}()
    t1[(1, 2)] = 1
    t2 = IntervalTree{Int, Int}()
    t2[(1001, 1002)] = 2
    @fact isempty(collect(IntervalTrees.IterativeTreeIntersectionIterator(t1, t2))) => true
    @fact isempty(collect(IntervalTrees.SuccessiveTreeIntersectionIterator(t1, t2))) => true
end


facts("Insertion") do
    t = IntervalTrees.IntervalTree{Int, Int}()
    n = 100000

    context("random insertions") do
        for v in 1:n
            k = randinterval()
            t[k] = v
        end
        @fact issorted(collect(keys(t))) => true
        @fact validkeys(t) => true
        @fact validparents(t) => true
        @fact validsiblings(t) => true
    end

    context("ordered insertions") do
        for v in 1:n
            k = (v,rand(v:maxend))
            t[k] = v
        end
        @fact issorted(collect(keys(t))) => true
        @fact validkeys(t) => true
        @fact validparents(t) => true
        @fact validsiblings(t) => true
    end

    context("reverse ordered insertions") do
        for v in 1:n
            k = (maxend-v,rand((maxend-v):maxend))
            t[k] = v
        end
        @fact issorted(collect(keys(t))) => true
        @fact validkeys(t) => true
        @fact validparents(t) => true
        @fact validsiblings(t) => true
    end

    context("bulk insertion") do
        intervals = Interval{Int}[]
        for v in 1:n
            a, b = randinterval()
            push!(intervals, Interval{Int}(a, b))
        end
        sort!(intervals)
        values = collect(Int, 1:n)
        t = IntervalTrees.IntervalTree{Int, Int}(intervals, values)

        @fact issorted(collect(keys(t))) => true
        @fact validkeys(t) => true
        @fact validparents(t) => true
        @fact validsiblings(t) => true
    end
end


facts("Updates") do
    t = IntervalTrees.IntervalTree{Int, Int}()
    n = 100000
    ks = [randinterval() for _ in 1:n]
    for (v, k) in enumerate(ks)
        t[k] = v
    end

    shuffle!(ks)
    for (v, k) in enumerate(ks)
        t[k] = v
    end

    @fact issorted(collect(keys(t))) => true
    @fact validkeys(t) => true
    @fact validparents(t) => true
    @fact validsiblings(t) => true
end


facts("Deletion") do
    B = 64
    t = IntervalTrees.IntervalBTree{Int, Int, B}()
    n = 100000

    # insert n random intervals
    intervals = [randinterval() for _ in 1:n]
    for (i, interval) in enumerate(intervals)
        t[interval] = i
    end
    shuffle!(intervals)

    # delete non-existant is a nop
    @fact delete!(t, (-1, -1)).n => n

    # delete one
    interval = pop!(intervals)
    delete!(t, interval)
    @fact validkeys(t) => true
    @fact validparents(t) => true
    @fact validsiblings(t) => true
    @fact issorted(collect(keys(t))) => true
    @fact haskey(t, interval) => false
    @fact all(map(interval -> haskey(t, interval), intervals)) => true

    # delete a random 50%
    for interval in 1:(n-1)/2
        interval = pop!(intervals)
        delete!(t, interval)
    end
    @fact validkeys(t) => true
    @fact validparents(t) => true
    @fact validsiblings(t) => true
    @fact issorted(collect(keys(t))) => true
    @fact all(map(interval -> haskey(t, interval), intervals)) => true

    # delete the rest
    for interval in intervals
        delete!(t, interval)
    end
    @fact isempty(t) => true
    @fact validkeys(t) => true
    @fact validparents(t) => true
    @fact validsiblings(t) => true

    # delete left-to-right
    shuffle!(intervals)
    for (i, interval) in enumerate(intervals)
        t[interval] = i
    end
    sort!(intervals)
    for interval in intervals
        delete!(t, interval)
    end
    @fact isempty(t) => true
    @fact validkeys(t) => true
    @fact validparents(t) => true
    @fact validsiblings(t) => true

    # delete right-to-left
    shuffle!(intervals)
    for (i, interval) in enumerate(intervals)
        t[interval] = i
    end
    reverse!(intervals)
    for interval in intervals
        delete!(t, interval)
    end
    @fact isempty(t) => true
    @fact validkeys(t) => true
    @fact validparents(t) => true
    @fact validsiblings(t) => true

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
    @fact isempty(t) => true
    @fact validkeys(t) => true
    @fact validparents(t) => true
    @fact validsiblings(t) => true

    # contrive a special case: merge with the left when we have a non-null
    # right
    t = IntervalBTree{Int, Int, 4}()
    for i in 1:24
        t[(i, i)] = i
    end
    @fact isa(t.root, InternalNode) => true
    @fact length(t.root.children) => 3
    @fact length(t.root.children[2].children) => 2
    delete!(t, (15, 15))
    keys_to_delete = collect(keys(t))
    for key in keys_to_delete
        delete!(t, key)
    end
    @fact isempty(t) => true
    @fact validkeys(t) => true
    @fact validparents(t) => true
    @fact validsiblings(t) => true
end


facts("Slices") do
    xs = Slice{Int, 10}()

    @fact_throws xs[0]
    @fact_throws xs[11]
    @fact_throws xs[0] = 1
    @fact_throws xs[11] = 1
    @fact_throws pop!(xs)
    @fact_throws insert!(xs, 0, 1)

    for x in 1:10
        push!(xs, x)
    end

    @fact_throws push!(xs, 11)
    @fact_throws insert!(xs, 11)
    @fact_throws resize!(xs, 11)
    @fact_throws splice!(xs, 0)
    @fact_throws splice!(xs, 11)
end


# Abuse low-level functions to test cases that wouldn't otherwise occur
facts("Low Level") do
    # findidx should return 0 when called on an empty node
    x = Interval{Int}(1, 1)
    node = InternalNode{Int, Int, 32}()
    @fact IntervalTrees.findidx(node, x) => 0
    node = LeafNode{Int, Int, 32}()
    @fact IntervalTrees.findidx(node, x) => 0
    @fact IntervalTrees.firstintersection(node, x) => (node, 0)

    # test that delete! still works on a contrived tree with one internal and
    # one leaf node
    t = IntervalBTree{Int, Int, 32}()
    t.root = InternalNode{Int, Int, 32}()
    push!(t.root.children, LeafNode{Int, Int, 32}())
    push!(t.root.keys, x)
    push!(t.root.children[1].keys, x)
    push!(t.root.children[1].values, 1)
    delete!(t, (1,1))
    @fact isa(t.root, LeafNode) => true

    # test that the right thing happens if you delete the last key in a non-root
    # leaf node (which can't actually happen)
    node = LeafNode{Int, Int, 32}()
    push!(node.keys, x)
    push!(node.values, 1)
    @fact IntervalTrees.findidx(node, x) => 1
end

exitstatus()

end


