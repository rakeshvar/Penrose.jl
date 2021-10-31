include("tiles.jl")
include("svg.jl")

function dontimes(tiles, name, n)
    tilesn = inflaten(tiles, n)
    for i in 0:n
        svg(tilesn[i+1], "$name-$i.svg")
    end
end

a = [Fatt(0, 1)]
# dontimes(a, "a", 6)

b = [Tall(0, 1)]
# dontimes(b, "b", 6)

cps = [ρ(1, i) for i in 0:11]

function circle()
    circletiles = [[Tall(cps[i+1], cps[i], Val(:down)) for i in 1:2:10]...,
                   [Tall(cps[i], cps[i+1], Val(:up  )) for i in 2:2:10]...]

    config[:scale] = 1200
    config[:Aarc_color] = "#ff5e25"
    config[:Barc_color] = "none"
    config[:Stile_color] = "#090"
    config[:Ltile_color] = "#9f3"
    config[:base_stroke_width] = 1/1200
    dontimes(circletiles, "c", 8)
end

starttiles = [[Fatt(0, cps[i], Val(:up  )) for i in 1:2:10]...,
              [Fatt(0, cps[i], Val(:down)) for i in 1:2:10]...]
dontimes(starttiles, "s", 7)