module Program

open CellGrid    

let random = System.Random()

let getRandomItemFromList list = List.item (random.Next(List.length list)) list

let optionListToList list =
    let rec optionListToList' list' acc =
        match list' with
        | [] -> acc
        | x::ls -> 
            match x with
            | None -> optionListToList' ls acc
            | Some obj -> optionListToList' ls (obj::acc)
    optionListToList' list []

let binaryTree (cellGrid : CellGrid) = 
    Seq.iter (fun row ->
        Seq.iter (fun cellPosition ->
        let neighborList = optionListToList [CellGrid.getNorth cellPosition cellGrid; CellGrid.getEast cellPosition cellGrid]
        match (List.length neighborList) with
            | 0 -> ()
            | x -> CellGrid.linkBidi cellPosition (getRandomItemFromList neighborList) cellGrid
        ) row
    ) (CellGrid.getRows cellGrid)    

let sidewinder (cellGrid : CellGrid) = 

    Seq.iteri (fun row rowArray ->
        let doCloseRun run = 
            let chosenCell = getRandomItemFromList run
            CellGrid.linkBidiIfNeighborCellExists CellGrid.getNorth chosenCell cellGrid
            []


        if (row = 0) then
            Seq.iter (fun cellPosition -> 
                CellGrid.linkBidiIfNeighborCellExists CellGrid.getEast cellPosition cellGrid;
            ) rowArray
        else 
            Seq.fold (fun run cell -> 
                    let newRun = cell :: run
                    match CellGrid.getEast cell cellGrid with
                    | None -> doCloseRun newRun
                    | Some _ ->
                        match random.Next(2) with
                        | 0 -> CellGrid.linkBidiIfNeighborCellExists CellGrid.getEast cell cellGrid
                               newRun
                        | 1 -> doCloseRun newRun
                        | _ -> raise (System.Exception("Issue selecting if to close the run"))
                ) [] rowArray  |> ignore
         ) (CellGrid.getRows cellGrid)
           
           

[<EntryPoint>]
let main argv =
    let cellGrid = CellGrid.create 5 10

    CellGrid.printGrid cellGrid;
    printf "\n";
    binaryTree cellGrid
    CellGrid.printGrid cellGrid;

    
    let cellGrid2 = CellGrid.create 20 20
    sidewinder cellGrid2

    CellGrid.printGrid cellGrid2

    0 // return an integer exit code
