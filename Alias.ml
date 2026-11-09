(* Alias module - command aliases *)

(* Expand alias in command *)
let expand_alias aliases command = 
  let rec find_alias cmd = function
    | [] -> cmd
    | (alias, expansion)::tl ->
        if String.starts_with ~prefix:(alias ^ " ") cmd 
           || cmd = alias then
          let rest = 
            if String.length cmd > String.length alias then
              String.sub cmd (String.length alias) (String.length cmd - String.length alias)
            else
              ""
          in
          expansion ^ rest
        else
          find_alias cmd tl
  in
  find_alias command aliases

(* Get alias expansion *)
let get_alias alias aliases = 
  List.assoc_opt alias aliases

(* Add alias to list *)
let add_alias alias command aliases = 
  (alias, command) :: (List.remove_assoc alias aliases)

(* Remove alias from list *)
let remove_alias alias aliases = 
  List.remove_assoc alias aliases

(* List all aliases *)
let list_aliases aliases = aliases
