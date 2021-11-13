module CellPosition

open GridDimension

// Simple unchecked packer for row and column
type RawCellPosition = {row: int; column: int}

type CellPosition = private Validated of RawCellPosition
type NeighborFunction = CellPosition -> GridDimension -> CellPosition option

let private validateRowNumber row maxHeight = row < maxHeight && row >= 0
let private validateColumnNumber column maxWidth = column < maxWidth && column >= 0

let create cellPosition validatedDimensions = 
    assert (validateRowNumber cellPosition.row (GridDimension.getHeight validatedDimensions))
    assert (validateColumnNumber cellPosition.column (GridDimension.getWidth validatedDimensions))
    (Validated cellPosition)

let getRow (Validated {row=row}) = row
let getColumn (Validated {column=column}) = column

let private (|Top|Bottom|Middle|) ({row=row}, validatedDimensions) =
    if row <= 0 then Top
    elif row >= (GridDimension.getHeight validatedDimensions) - 1 then Bottom
    else Middle
    
let private (|Leftmost|Rightmost|Middle|) ({column=column}, validatedDimensions) =
    if column <= 0 then Leftmost
    elif column >= (GridDimension.getWidth validatedDimensions) - 1 then Rightmost
    else Middle
    
let getWest (Validated cellPosition) dimensions=
    match (cellPosition, dimensions) with
    | Leftmost -> None
    | _ -> Some(create {cellPosition with column=cellPosition.column - 1} dimensions)
            
let getEast (Validated cellPosition) dimensions =
    match (cellPosition, dimensions) with
    | Rightmost -> None
    | _ -> Some(create {cellPosition with column=cellPosition.column + 1} dimensions)
            
let getSouth (Validated cellPosition) dimensions =
    match (cellPosition, dimensions) with
    | Bottom -> None
    | _ -> Some(create {cellPosition with row=cellPosition.row + 1} dimensions)
            
let getNorth (Validated cellPosition) dimensions =
    match (cellPosition, dimensions) with
    | Top -> None
    | _ -> Some(create {cellPosition with row=cellPosition.row - 1} dimensions)
    
    