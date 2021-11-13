module CellGrid
open Cell
open CellPosition
open GridDimension

type CellGrid = private {cells: Cell[,]; dimensions: GridDimension}

let private getCell cellPosition cellGrid = cellGrid.cells.[CellPosition.getRow cellPosition, CellPosition.getColumn cellPosition]

let private getANeighbour (neighbourFn: NeighborFunction) pos cellGrid = neighbourFn pos (cellGrid.dimensions)

let getNorth = getANeighbour CellPosition.getNorth
let getEast = getANeighbour CellPosition.getEast
let getWest = getANeighbour CellPosition.getWest 
let getSouth = getANeighbour CellPosition.getSouth

let create height width = 
    let dimensions = GridDimension.create height width
    {dimensions=dimensions; cells=Array2D.init height width (fun row column -> Cell.create (CellPosition.create {row=row; column=column} dimensions))}

let private getColumn columnIndex cellGrid = List.init (GridDimension.getHeight cellGrid.dimensions) (fun row -> CellPosition.create {row=row; column=columnIndex} cellGrid.dimensions)
let private getRow rowIndex cellGrid = List.init (GridDimension.getWidth cellGrid.dimensions) (fun column -> CellPosition.create {row=rowIndex; column=column} cellGrid.dimensions)
    
let getRows cellGrid = ((List.map (fun row -> getRow row cellGrid)) [0..GridDimension.getHeight cellGrid.dimensions - 1])
let getColumns cellGrid = ((List.map (fun column -> getColumn column cellGrid)) [0..GridDimension.getWidth cellGrid.dimensions - 1])

let neighbours cellPosition {dimensions=dimensions} = [CellPosition.getNorth cellPosition dimensions; CellPosition.getSouth cellPosition dimensions; CellPosition.getEast cellPosition dimensions; CellPosition.getWest cellPosition dimensions]

let private link c1 c2 (cellGrid : CellGrid) = 
    assert (List.length (neighbours c1 cellGrid |> List.filter (Some c2).Equals) = 1)
    cellGrid.cells.[CellPosition.getRow c1, CellPosition.getColumn c1] <- Cell.addLink (getCell c1 cellGrid) c2

let linkBidi c1 c2 cellGrid = 
    link c1 c2 cellGrid
    link c2 c1 cellGrid

let applyIfNeighborCellExist determinant fn cellPosition (cellGrid: CellGrid) = 
    match (determinant cellPosition cellGrid) with
    | None -> ()
    | Some cellPosition' -> fn (cellPosition : CellPosition) (cellPosition': CellPosition) cellGrid
    
let linkBidiIfNeighborCellExists determinant cellPosition cellGrid = applyIfNeighborCellExist determinant linkBidi cellPosition cellGrid

let printGrid cellGrid = 
    List.iter (fun x -> printf "_")  [0..(GridDimension.getWidth cellGrid.dimensions) * 2]
    printf "\n"
    List.iter
        (fun row -> 
            printf "|"
            List.iter (fun cellPosition -> 
                let cell = getCell cellPosition cellGrid
                match (CellPosition.getSouth cellPosition cellGrid.dimensions) with
                | None -> printf "_"
                | Some c' -> if (Cell.linked cell c') then printf " " else printf "_"
                match (CellPosition.getEast cellPosition cellGrid.dimensions) with
                | None -> printf "|"
                | Some c' -> if (Cell.linked cell c') then printf "_" else printf "|"
                ) row
            printf "\n"
        ) (getRows cellGrid)
    

