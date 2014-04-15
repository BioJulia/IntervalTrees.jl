
module TestIntervalTrees

using FactCheck
using IntervalTrees


# Generating random intervals
srand(12345)
const maxend = int(1e9)

function randinterval()
    a = rand(1:maxend)
    b = rand(a:maxend)
    return (a, b)
end


# Verify that internal node keys are correct
function isvalid(node::IntervalTrees.IntervalBTree)
    return isvalid(node.root, 0, maxend, maxend)
end


function isvalid(node::IntervalTrees.InternalNode, minstart, maxstart, maxend)
    for i in 1:length(node.keys)
        k = node.keys[i]
        if !isvalid(node.children[i], minstart, k[1], k[2]) ||
           !isvalid(node.children[i+1], k[1], maxstart, k[2])
           return false
       end
    end
    return true
end


function isvalid(node::IntervalTrees.LeafNode, minstart, maxstart, maxend)
    for (keystart, keyend) in node.keys
        if !(minstart <= keystart < maxstart) || keyend > maxend
            return false
        end
    end
    return true
end



facts("Searching") do
    t = IntervalTree{Int, Int}()

    @fact haskey(t, (1,2)) => false
end


facts("Iteration") do
    t = IntervalTree{Int, Int}()
    @fact isempty(collect(t)) => true
end


facts("Insertion") do
    t = IntervalTree{Int, Int}()
    n = 200

    context("random insertions") do
        for v in 1:n
            k = randinterval()
            t[k] = v
        end
        @fact issorted(collect(keys(t))) => true
        @fact isvalid(t) => true
    end

    context("ordered insertions") do
        for v in 1:n
            k = (v,rand(v:maxend))
            t[k] = v
        end
        @fact issorted(collect(keys(t))) => true
        @fact isvalid(t) => true
    end

    context("reverse ordered insertions") do
        for v in 1:n
            k = (maxend-v,rand((maxend-v):maxend))
            t[k] = v
        end
        @fact issorted(collect(keys(t))) => true
        @fact isvalid(t) => true
    end
end


facts("Deletion") do

end


end

