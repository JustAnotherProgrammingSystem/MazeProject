module Cell

open CellPosition

type Cell = private {cellPosition: CellPosition; links: CellPosition list}

let create cellPosition = {cellPosition=cellPosition; links=[]}

let getCellPosition ({cellPosition = pos}: Cell) = pos;

let linked c1 c2Position = List.contains c2Position c1.links

let addLink c1 c2Position = {c1 with links = c2Position :: c1.links} 

let removeLink c1 c2Position = {c1 with links = List.except [c2Position] c1.links}

