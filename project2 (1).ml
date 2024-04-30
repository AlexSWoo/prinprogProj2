let validVarNames = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";"n";"o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z"];;

let partition (input: string list) (bound : string) : string list list =
  let rec aux acc current = function
    | [] -> List.rev (List.rev current :: acc)
    | h :: t when h = bound -> aux (List.rev current :: acc) [] t
    | h :: t -> aux acc (h :: current) t
  in
  aux [] [] input;;


let getVariables (input : string list) : string list = 
	let rec aux acc = function
	| [] -> List.rev acc
	| hd :: tl when List.mem hd validVarNames && not (List.mem hd acc) -> aux (hd :: acc) tl
	| _ :: tl -> aux acc tl
	in
	aux [] input
;;

let rec generateDefaultAssignments (varList : string list) : (string * bool) list = 
  match varList with
  | [] -> []
  | hd :: tl -> (hd, false) :: generateDefaultAssignments tl
;;

let rec generateNextAssignments (assignList : (string * bool) list) : (string * bool) list * bool =
  let rec aux carry lst =
    match lst with
    | [] -> ([], carry)
    | (var, value) :: tl ->
        if value && carry then
          let (rest, carry') = aux true tl in
          ((var, false) :: rest, carry')
        else if carry then
          ((var, true) :: tl, false)
        else
          ((var, value) :: tl, false)
  in
  let (lst', carry) = aux true (List.rev assignList) in
  (List.rev lst', carry);;

let rec lookupVar (assignList : (string * bool) list) (str : string) : bool =
  match assignList with
  | [] -> raise (Invalid_argument "String not found in the list")
  | (s, b)::rest -> if s = str then b else lookupVar rest str
;;

let partition2 (input: string list) (bound : string) : string list * string list =
  let rec aux acc current = function
    | [] -> (List.rev current, List.rev acc)
    | h :: t when h = bound -> (List.rev current, t)
    | h :: t -> aux acc (h :: current) t
  in
  aux [] [] input;;

let buildCNF (input : string list) : (string * string) list list =
  let rec helper acc = function
    | [] -> List.rev acc
    | "("::t -> 
        let clause, rest = partition2 t ")" in
        helper ((process_clause clause) :: acc) ("AND"::rest)
    | "AND"::t -> helper acc t
    | _ -> failwith "Invalid input"
  and process_clause clause =
    let rec aux acc = function
      | [] -> List.rev acc
      | "OR"::t -> aux acc t
      | "NOT"::var::t -> aux ((var, "NOT") :: acc) t
      | var::t -> aux ((var, "") :: acc) t
    in
    aux [] clause
  in
  helper [] input
;;

let evaluateCNF (t : (string * string) list list) (assignList : (string * bool) list) : bool =
  let lookup var = 
    try List.assoc var assignList with Not_found -> false in
  let evaluateLiteral (var, negation) = 
    let value = lookup var in
    if negation = "NOT" then not value else value in
  let evaluateClause clause = 
    List.exists evaluateLiteral clause in
  List.for_all evaluateClause t
;;

let satisfy (input : string list) : (string * bool) list =
  let rec helper assignList =
    let cnf = buildCNF input in
    if evaluateCNF cnf assignList then assignList
    else
      let nextAssignList, carry = generateNextAssignments assignList in
      if carry then failwith "No satisfying assignment found"
      else helper nextAssignList
  in
  let vars = List.filter ((<>) "AND") (List.filter ((<>) "OR") (List.filter ((<>) "NOT") input)) in
  let initialAssignList = List.map (fun var -> (var, false)) vars in
  helper initialAssignList
;;
