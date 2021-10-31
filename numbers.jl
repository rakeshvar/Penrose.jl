import Base: +, *, /, ==, -
using Test

const UIR = Union{Integer, Rational}

# struct A{R<:Rational} <: Real
#     x::R
#     y::R
# end

struct A <: Real
    x::Rational
    y::Rational
end

# A(x::Rational, y::Rational) = A(promote(x, y)...)
# A(x::UIR, y::UIR) = A(Rational(x), Rational(y))
A(x::UIR) = A(x, zero(x))

const r5 = A(0, 1)
const ComplexA  = Complex{A}

(::Type{T})(a::A) where {T<:AbstractFloat} = T(a.x + √5 * a.y)

*(a1::A, a2::A) = A(a1.x*a2.x+5*a1.y*a2.y, a1.x*a2.y+a1.y*a2.x)
+(a1::A, a2::A) = A(a1.x+a2.x, a1.y*a2.y)
/(a::A, n::UIR) = A(a.x//n, a.y//n)

==(a::A, x::T) where T<:AbstractFloat = T(a) == x
==(x::T, a::A) where T<:AbstractFloat = T(a) == x
-(a::A) = A(-a.x, -a.y)

ϕ = A(1, 1)/2
ψ = A(-1, 1)/2

@test ϕ == (√5+1.)/2
@test ψ == (√5-1.)/2
@show A(0) zero(ϕ) ϕ ψ

#=

promote_rule(::Type{Complex{T}}, ::Type{S}) where {T<:Real,S<:Real} =
    Complex{promote_type(T,S)}
promote_rule(::Type{Complex{T}}, ::Type{Complex{S}}) where {T<:Real,S<:Real} =
    Complex{promote_type(T,S)}


function show(io::IO, z::Complex)
    r, i = reim(z)
    compact = get(io, :compact, false)
    show(io, r)
    if signbit(i) && !isnan(i)
        print(io, compact ? "-" : " - ")
        if isa(i,Signed) && !isa(i,BigInt) && i == typemin(typeof(i))
            show(io, -widen(i))
        else
            show(io, -i)
        end
    else
        print(io, compact ? "+" : " + ")
        show(io, i)
    end
    if !(isa(i,Integer) && !isa(i,Bool) || isa(i,AbstractFloat) && isfinite(i))
        print(io, "*")
    end
    print(io, "im")
end
show(io::IO, z::Complex{Bool}) =
    print(io, z == im ? "im" : "Complex($(z.re),$(z.im))")

function show_unquoted(io::IO, z::Complex, ::Int, prec::Int)
    if operator_precedence(:+) <= prec
        print(io, "(")
        show(io, z)
        print(io, ")")
    else
        show(io, z)
    end
end

function read(s::IO, ::Type{Complex{T}}) where T<:Real
    r = read(s,T)
    i = read(s,T)
    Complex{T}(r,i)
end
function write(s::IO, z::Complex)
    write(s,real(z),imag(z))
end

## byte order swaps: real and imaginary part are swapped individually
bswap(z::Complex) = Complex(bswap(real(z)), bswap(imag(z)))

## equality and hashing of complex numbers ##

==(z::Complex, w::Complex) = (real(z) == real(w)) & (imag(z) == imag(w))
==(z::Complex, x::Real) = isreal(z) && real(z) == x
==(x::Real, z::Complex) = isreal(z) && real(z) == x

+(z::Complex) = Complex(+real(z), +imag(z))
-(z::Complex) = Complex(-real(z), -imag(z))
+(z::Complex, w::Complex) = Complex(real(z) + real(w), imag(z) + imag(w))
-(z::Complex, w::Complex) = Complex(real(z) - real(w), imag(z) - imag(w))
*(z::Complex, w::Complex) = Complex(real(z) * real(w) - imag(z) * imag(w),
                                    real(z) * imag(w) + imag(z) * real(w))

muladd(z::Complex, w::Complex, x::Complex) =
    Complex(muladd(real(z), real(w), real(x)) - imag(z)*imag(w), # TODO: use mulsub given #15985
            muladd(real(z), imag(w), muladd(imag(z), real(w), imag(x))))

# adding or multiplying real & complex is common
+(x::Real, z::Complex) = Complex(x + real(z), imag(z))
+(z::Complex, x::Real) = Complex(x + real(z), imag(z))
function -(x::Real, z::Complex)
    # we don't want the default type for -(Bool)
    re = x - real(z)
    Complex(re, - oftype(re, imag(z)))
end
-(z::Complex, x::Real) = Complex(real(z) - x, imag(z))
*(x::Real, z::Complex) = Complex(x * real(z), x * imag(z))
*(z::Complex, x::Real) = Complex(x * real(z), x * imag(z))

muladd(x::Real, z::Complex, y::Number) = muladd(z, x, y)
muladd(z::Complex, x::Real, y::Real) = Complex(muladd(real(z),x,y), imag(z)*x)
muladd(z::Complex, x::Real, w::Complex) =
    Complex(muladd(real(z),x,real(w)), muladd(imag(z),x,imag(w)))
muladd(x::Real, y::Real, z::Complex) = Complex(muladd(x,y,real(z)), imag(z))
muladd(z::Complex, w::Complex, x::Real) =
    Complex(muladd(real(z), real(w), x) - imag(z)*imag(w), # TODO: use mulsub given #15985
            muladd(real(z), imag(w), imag(z) * real(w)))

/(a::R, z::S) where {R<:Real,S<:Complex} = (T = promote_type(R,S); a*inv(T(z)))
/(z::Complex, x::Real) = Complex(real(z)/x, imag(z)/x)

function /(a::Complex{T}, b::Complex{T}) where T<:Real
    are = real(a); aim = imag(a); bre = real(b); bim = imag(b)
    if abs(bre) <= abs(bim)
        if isinf(bre) && isinf(bim)
            r = sign(bre)/sign(bim)
        else
            r = bre / bim
        end
        den = bim + r*bre
        Complex((are*r + aim)/den, (aim*r - are)/den)
    else
        if isinf(bre) && isinf(bim)
            r = sign(bim)/sign(bre)
        else
            r = bim / bre
        end
        den = bre + r*bim
        Complex((are + aim*r)/den, (aim - are*r)/den)
    end
end

inv(z::Complex{<:Union{Float16,Float32}}) =
    oftype(z, inv(widen(z)))

/(z::Complex{T}, w::Complex{T}) where {T<:Union{Float16,Float32}} =
    oftype(z, widen(z)*inv(widen(w)))

^(z::Complex{T}, p::Complex{T}) where T<:Real = _cpow(z, p)
^(z::Complex{T}, p::T) where T<:Real = _cpow(z, p)
^(z::T, p::Complex{T}) where T<:Real = _cpow(z, p)

^(z::Complex, n::Bool) = n ? z : one(z)
^(z::Complex, n::Integer) = z^Complex(n)

^(z::Complex{<:AbstractFloat}, n::Bool) = n ? z : one(z)  # to resolve ambiguity
^(z::Complex{<:Integer}, n::Bool) = n ? z : one(z)        # to resolve ambiguity

^(z::Complex{<:AbstractFloat}, n::Integer) =
    n>=0 ? power_by_squaring(z,n) : power_by_squaring(inv(z),-n)
^(z::Complex{<:Integer}, n::Integer) = power_by_squaring(z,n) # DomainError for n<0

function ^(z::Complex{T}, p::S) where {T<:Real,S<:Real}
    P = promote_type(T,S)
    return Complex{P}(z) ^ P(p)
end
function ^(z::T, p::Complex{S}) where {T<:Real,S<:Real}
    P = promote_type(T,S)
    return P(z) ^ Complex{P}(p)
end

=#