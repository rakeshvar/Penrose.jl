import Base: +, -, *, /

abstract type Tile end

# ρ rotate by n * π/5
ρ(t::Number, n::Integer)  = t * exp(im * n * pi/5)
ρ(t::T, n::Integer) where T<:Tile = T(ρ(t.a, n), ρ(t.b, n), ρ(t.c, n))

ϕ = (√5 + 1)/2
ψ = ϕ - 1
split(a, b, q, p) = (a*p+b*q)/(p+q)

getfattc(a, b, ::Val{:up}) = a + ρ(b-a, 1) * ψ
gettallc(a, b, ::Val{:up}) = a + ρ(b-a, 2) * ϕ
getfattc(a, b, ::Val{:down}) = a + ρ(b-a, -1) * ψ
gettallc(a, b, ::Val{:down}) = a + ρ(b-a, -2) * ϕ

struct Fatt{T<:Complex{F}} <: Tile 
    a::T
    b::T
    c::T
end

Fatt(a, b, v::Val=Val(:up)) = Fatt(a, b, getfattc(a, b, v))

struct Tall{T<:Complex{F}} <: Tile
    a::T
    b::T
    c::T
end

Tall(a, b, v::Val=Val(:up)) = Tall(a, b, gettallc(a, b, v))

function inflate(t::Fatt)
    e = split(t.a, t.b, ϕ, 1)
    d = split(t.a, t.c, ϕ, 1)
    [Fatt(e, t.a, d), Tall(d, t.c, e), Fatt(t.b, t.c, e)]
end

function inflate(t::Tall)
    d = split(t.a, t.c, 1, ϕ)
    [Tall(d, t.a, t.b), Fatt(t.b, t.c, d)]
end

inflate(tiles::AbstractArray{<:Tile}) = vcat(inflate.(tiles)...)

function inflaten(tiles::AbstractArray{<:Tile}, n)
    ret = Vector{Tile}[tiles]
    for i in 1:n
        push!(ret, inflate(ret[end]))
    end
    ret
end

points(a::Tile) = [a.a, a.b, a.c]
points(tiles::AbstractVector{<:Tile}) = vcat(points.(tiles)...)
center(a::Tile) = (a.a+a.b)/2

scaletoint(c::Real, scale) = round(Int, scale*c)
scaletoint(c::Complex, scale) = Complex(round.(Int, scale .* reim(c) )...)
scaletoint(t::T, scale) where T<:Tile = T(scaletoint.((t.a, t.b, t.c), scale)...)