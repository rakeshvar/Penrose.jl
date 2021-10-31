
struct A{T<:Complex}
    x::T
    y::T
end

A(x) = A(x, zero(x))

@show A(1. + 1. * im, 1. -1. * im)
@show A(1+1im, 1-1im)
@show A(1. +1im, 1-1im)

@show A(1+1im, 1-1im)
@show A(1+1im, 1im)
@show A(1+1im, 1)

@show A(1, 1-1im)
@show A(im, 1im)

@show A(1., 1.)
@show A(1, 1.)
@show A(1., 1)
@show A(1, 1)

@show A(1.)
@show A(1)