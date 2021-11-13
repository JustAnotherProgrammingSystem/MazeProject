module GridDimension

type private RawGridDimension = {height: int; width: int}

type GridDimension = private Validated of RawGridDimension

let create height width =
    assert (height > 0)
    assert (width > 0)
    Validated {height=height; width=width}

let getHeight (Validated {height=height}) = height

let getWidth (Validated {width=width}) = width
