
config = Dict(
    :scale => 1000,
    :width => "100%", 
    :height => "100%",
    :stroke_color => "#fff",
    :stroke_width => 1,
    :margin => .05,
    :tile_opacity => 0.8,
    # :random_tile_colors => false,
    :Stile_color => "#08f",
    :Ltile_color => "#0035f3",
    :Aarc_color => "#f00",
    :Barc_color => "#00f",
    :draw_tiles => true,
    :draw_arcs => true,
    # :reflect_x => true,
    :draw_rhombuses => true,
    # :rotate => 0,
    # :flip_y => false, 
    # :flip_x => false,
)

tile_color(a::Fatt) = config[:Stile_color]
tile_color(a::Tall) = config[:Ltile_color]
tile_opacity(a) = config[:tile_opacity]


function viewbox(tiles, margin)
    # Find the range of the image
    allpoints = points(tiles)
    xmin, xmax = extrema(real(p) for p in allpoints)
    ymin, ymax = extrema(-imag(p) for p in allpoints)
    xmin -= margin; xmax += margin
    ymin -= margin; ymax += margin
    width = xmax - xmin
    height = ymax - ymin
    "$xmin $ymin $width $height"
end

function centerlt(t1, t2)
    c1, c2 = center(t1), center(t2)
    (real(c1) ≈ real(c2)) ? (imag(c1) < imag(c2)) : (real(c1) < real(c2))
end


function svg(tiles, name, config=config)
    scale = config[:scale]
    margin = config[:margin]

    if config[:draw_rhombuses]
        sort!(tiles, lt=centerlt)
        centers = centers.(tiles)
        keep = [true, (centers[2:end] .≉ centers[1:end-1])...]
        tiles = tiles[keep]
    end

    tiles = scaletoint.(tiles, scale)
    viewboxx = viewbox(tiles, round(Int, margin*scale))

    ri(a) = (real(a), -imag(b))

    function tile_path(t::Tile)
        ax, ay = ri(t.a)
        bx, by = ri(t.b)
        cx, cy = ri(t.c)
        if config[:draw_rhombuses]
            dx, dy = ri(t.a+t.b-t.c)
            "M$ax,$ay L$cx,$cy L$bx,$by L$dx,$dy Z"
        else
            "M$ax,$ay L$cx,$cy L$bx,$by Z"
        end
    end

    function arc(a, b, c)
        st = (a+c)/2
        r = abs(c-a)/2
        en = a + (config[:draw_rhombuses] ? (b-c)/2 : r * (b-a) / abs(b-a))
        if real(st-a)*imag(en-a) < imag(st-a) * real(en-a)
            st, en = en, st
        end
        sx, sy = ri(st)
        ex, ey = ri(en)
        r = round(Int, r)
        "M $sx $sy A $r $r 0 0 0 $ex $ey" 
    end

    function tile_arcs(t::Tile)
        arc(t.a, t.b, t.c), arc(t.b, t.a, t.c)
    end

    Aarc_color, Barc_color = config[:Aarc_color], config[:Barc_color]

    if config[:draw_tiles]
        if config[:tilesvary]
            tilepaths = ["""<path fill="$(tile_color(t))" opacity="$(tile_opacity(t))" d="$(tile_path(t))"/>""" for t in tiles]
            tilepathsvg  = join(tilepaths, "\n")
        else
            tilepaths = tilepath.(tiles)
            types = isa.(tiles, Fatt)
            fpaths = """<path fill="$(config[:Stile_color])" d=" """ * join(tilepaths[types], "\n") * """ " />"""
            tpaths = """<path fill="$(config[:Ltile_color])" d=" """ * join(tilepaths[.!types], "\n") * """ " />"""
            tilepathsvg = fpaths * tpaths
        end
    else
        tilepathsvg = ""
    end

    if config[:draw_arcs]
        arcs = tile_arcs(t)
        aarcsvg = """<path fill="none" stroke="$Aarc_color" d=" """ * join(first.(arcs), "\n") * """ "/>"""
        barcsvg = """<path fill="none" stroke="$Barc_color" d=" """ * join( last.(arcs), "\n") * """ "/>"""
        arcpathssvg = aarcsvg * barcsvg
    else
        arcpathssvg = ""
    end

    head = """<svg width="100%" height="100%" viewBox="$viewboxx" xmlns="http://www.w3.org/2000/svg">"""
    head *= """<g style="stroke:$(config[:stroke_color]); stroke-width: $(config[:stroke_width]);">"""
    tail = "</g>\n</svg>"
    svg = head * tilepathsvg * arcpathssvg * tail

    open(name, "w") do io
        write(io, svg)
    end
end
