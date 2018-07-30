# This is very simple implementation of go-style slices. Nodes in the B-tree
# have fixed sized arrays that change when elements are inserted or deleted.
# We would like for those to behave as though they are resized without actually
# doing any reallocation.

using Compat

mutable struct Slice{T, N} <: AbstractVector{T}
    data::Vector{T}
    n::Int # Number of stored elements

    function Slice{T,N}() where {T,N}
        return new{T,N}(Vector{T}(undef, N), 0)
    end
end

function Base.length(s::Slice)
    return s.n
end


function Base.size(s::Slice)
    return (s.n,)
end

function Base.getindex(s::Slice{T, N}, i::Integer) where {T, N}
    if 1 <= i <= s.n
        @inbounds x = s.data[i]
        return x
    else
        throw(BoundsError())
    end
end


@inline function unsafe_getindex(s::Slice{T, N}, i::Integer) where {T, N}
    @inbounds x = s.data[i]
    return x
end


function Base.setindex!(s::Slice{T, N}, value, i::Integer) where {T, N}
    if 1 <= i <= s.n
        @inbounds s.data[i] = value
        return value
    else
        throw(BoundsError())
    end
end


function Base.push!(s::Slice{T, N}, value) where {T, N}
    if s.n < N
        s.n += 1
        @inbounds s.data[s.n] = value
    else
        throw(BoundsError("Slice expanded past fixed size."))
    end
end


function Base.pop!(s::Slice{T, N}) where {T, N}
    if s.n > 0
        @inbounds x = s.data[s.n]
        s.n -= 1
        return x
    else
        throw(BoundsError())
    end
end


function Base.insert!(s::Slice{T, N}, i::Integer, value) where {T, N}
    if s.n < N && 1 <= i <= s.n + 1
        # TODO: This should work but won't. Fix this in Julia.
        #copy!(s.data, i+1, s.data, i, s.n - i + 1)
        if isbits(T)
            unsafe_copyto!(pointer(s.data, i+1), pointer(s.data, i), s.n - i + 1)
        else
            for k in 0:(s.n - i)
                @inbounds s.data[s.n+1-k] = s.data[s.n-k]
            end
        end
        @inbounds s.data[i] = value
        s.n += 1
        return s
    else
        throw(BoundsError())
    end
end


function Base.resize!(s::Slice{T, N}, n::Integer) where {T, N}
    if 1 <= n <= N
        s.n = n
        return s
    else
        throw(BoundsError())
    end
end


function Base.splice!(s::Slice{T, N}, i::Integer) where {T, N}
    if 1 <= i <= s.n
        @inbounds x = s.data[i]
        for j in i:s.n-1
            @inbounds s.data[j] = s.data[j + 1]
        end
        s.n -= 1
        return x
    else
        throw(BoundsError())
    end
end


function Base.searchsortedfirst(s::Slice, x)
    return searchsortedfirst(s.data, x, 1, s.n, Base.Order.Forward)
end



function slice_insert!(xs::Vector{T}, count::Integer, i::Integer, value::T) where T
    if count < length(xs) && 1 <= i <= count + 1
        if isbits(T)
            unsafe_copyto!(pointer(xs, i+1), pointer(xs, i), count - i + 1)
        else
            for k in 0:(count - i)
                @inbounds xs[count+1-k] = xs[count-k]
            end
        end
        @inbounds xs[i] = value
        count += 1
        return count
    else
        throw(BoundsError())
    end
end


function slice_splice!(xs::Vector{T}, count::Integer, i::Integer) where T
    if 1 <= i <= count
        @inbounds x = xs[i]
        for j in i:count-1
            @inbounds xs[j] = xs[j + 1]
        end
        count -= 1
        return x, count
    else
        throw(BoundsError())
    end
end
