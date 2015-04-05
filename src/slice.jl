
# This is very simple implementation of go-style slices. Nodes in the B-tree
# have fixed sized arrays that change when elements are inserted or deleted.
# We would like for those to behave as though they are resized without actually
# doing any reallocation.

type Slice{T, N} <: AbstractVector{T}
    data::Vector{T}
    n::Int # Number of stored elements

    function Slice()
        new(Array(T, N), 0)
    end
end

function Base.length(s::Slice)
    return s.n
end


function Base.size(s::Slice)
    return (s.n,)
end


function Base.getindex{T, N}(s::Slice{T, N}, i::Integer)
    if 1 <= i <= s.n
        @inbounds x = s.data[i]
        return x
    else
        throw(BoundsError)
    end
end


function Base.setindex!{T, N}(s::Slice{T, N}, value, i::Integer)
    if 1 <= i <= s.n
        @inbounds s.data[i] = value
        return value
    else
        throw(BoundsError)
    end
end


function Base.push!{T, N}(s::Slice{T, N}, value)
    if s.n < N
        s.n += 1
        @inbounds s.data[s.n] = value
    else
        throw(BoundsError("Slice expanded past fixed size."))
    end
end


function Base.pop!{T, N}(s::Slice{T, N})
    if s.n > 0
        @inbounds x = s.data[s.n]
        s.n -= 1
        return x
    else
        throw(BoundsError)
    end
end


function Base.insert!{T, N}(s::Slice{T, N}, i::Integer, value)
    if s.n < N && 1 <= i <= s.n + 1
        # TODO: This should work but won't. Fix this in Julia.
        #copy!(s.data, i+1, s.data, i, s.n - i + 1)
        if isbits(T)
            unsafe_copy!(pointer(s.data, i+1), pointer(s.data, i), s.n - i + 1)
        else
            for k in 0:(s.n - i)
                @inbounds s.data[s.n+1-k] = s.data[s.n-k]
            end
        end
        @inbounds s.data[i] = value
        s.n += 1
        return s
    else
        throw(BoundsError)
    end
end


function Base.resize!{T, N}(s::Slice{T, N}, n::Integer)
    if 1 <= n <= N
        s.n = n
        return s
    else
        throw(BoundsError)
    end
end


function Base.splice!{T, N}(s::Slice{T, N}, i::Integer)
    if 1 <= i <= s.n
        @inbounds x = s.data[i]
        for j in i:s.n-1
            @inbounds s.data[j] = s.data[j + 1]
        end
        s.n -= 1
        return x
    else
        throw(BoundsError)
    end
end


function Base.searchsortedfirst(s::Slice, x)
    return searchsortedfirst(s.data, x, 1, s.n, Base.Order.Forward)
end


