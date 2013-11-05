open Types
type pos = Lexing.position

type id = string


module NKT = Types

type width = int

type typ =
  | TPred
  | THdr of width
  | TInt of width
  | TPol
  | TFun of typ list * typ



type exp =
  | Id        of pos * id
  | Let       of pos * id * exp * exp
  | Fun       of pos * id list * exp
  | App       of pos * exp * exp list
  | If        of pos * exp * exp * exp
  | Par       of pos * exp * exp
  | Seq       of pos * exp * exp
  | Mod       of pos * exp * exp
  | Filter    of pos * exp
  | True      of pos
  | False     of pos
  | Test      of pos * exp * exp
  | And       of pos * exp * exp
  | Or        of pos * exp * exp
  | Neg       of pos * exp
  | Header    of pos * header
  | HeaderVal of pos * header_val
  | TypeIs    of pos * exp * typ



let sprintf = Format.sprintf

let string_of_pos pos = 
  let open Lexing in
  sprintf "%s, line %d, column %d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol)

module V : sig 
  type env 

  type value =
    | Header    of header
    | HeaderVal of header_val
    | Policy    of policy
    | Closure   of id list * exp * env
    | Predicate of pred

  val env_add : id -> value -> env -> env

  val env_lookup : id -> env -> value

  val empty_env : env

end = struct

  module Env = Map.Make (String)

  (* TODO : Redundant, can it be removed? *)
  type value = 
    | Header    of header
    | HeaderVal of header_val
    | Policy    of policy
    | Closure   of id list * exp * env
    | Predicate of pred

  and env = value Env.t

  (* type env = value Env.t *)

  let env_add    = Env.add
  let env_lookup = Env.find
  let empty_env  = Env.empty

end


type env = V.env

let env_add    = V.env_add
let env_lookup = V.env_lookup
let empty_env  = V.empty_env


exception Eval_error of string

let to_pol (v : V.value) : policy = match v with
  | V.Policy pol -> pol
  | V.Predicate pred -> Types.Filter pred
  | _ -> raise (Eval_error "expected policy or predicate")

let rec eval_helper (env : env) (e : exp) : V.value = 

  match e with

    | Id (p, x) -> (try env_lookup x env 
                    with Not_found -> raise 
                                        (Eval_error 
					  (sprintf "%s: Id : %s is undefined" (string_of_pos p) x )))

    | Let (_, x, with_e, in_e) -> 
        let env' = env_add x (eval_helper env with_e) env in eval_helper env' in_e

    | Fun (_, ids, body) -> V.Closure (ids, body, env)

    | App (p, f, args) ->
        (match eval_helper env f with
           | V.Closure (ids, body, env') -> 
               (try
                  let open List in
                  let eval_with_env = eval_helper env in
                  let env'' = fold_right2 env_add ids (map eval_with_env args) env' in
                    eval_helper env'' body
                with Invalid_argument _ -> raise 
                                             (Eval_error 
                                                (sprintf "%s: Mismatch in number of arguments" (string_of_pos p))))

           | _ -> raise 
                    (Eval_error 
                       (sprintf "%s: Expected a function" (string_of_pos p))))
        
    | If (p, pred, pol_true, pol_false) ->
        (match eval_helper env pred with
          | V.Predicate pre ->
              V.Policy (NKT.Par (NKT.Seq (NKT.Filter pre, 
                                          to_pol (eval_helper env pol_true)),
                                 NKT.Seq (NKT.Filter (NKT.Neg pre),
                                          to_pol (eval_helper env pol_false))))
          | _ -> raise 
                   (Eval_error 
                      (sprintf "%s: Mismatched types to if" (string_of_pos p))))


    | Par (p, e1, e2) -> 
        V.Policy (Types.Par (to_pol (eval_helper env e1),
                             to_pol (eval_helper env e2)))
    | Seq (p, e1, e2) ->
        V.Policy (Types.Seq (to_pol (eval_helper env e1),
                             to_pol (eval_helper env e2)))
    | Mod (p, e1, e2) -> 
       (match eval_helper env e1, eval_helper env e2 with
          | V.Header h, V.HeaderVal hv -> V.Policy (NKT.Mod (h, hv))
          | _ -> raise 
                   (Eval_error 
                     (sprintf "%s: Mismatched types to \":=\"" (string_of_pos p))))

    | Filter (p, e') -> eval_helper env e'
    | True _ -> V.Predicate NKT.True
    | False _ -> V.Predicate NKT.False
    | Test (p, e1, e2) ->
        (match eval_helper env e1, eval_helper env e2 with
          | V.Header h, V.HeaderVal hv -> V.Predicate (NKT.Test (h, hv))
          | _ -> raise 
                   (Eval_error 
                      (sprintf "%s: Mismatched types to \"=\"" (string_of_pos p))))

    | And (p, e1, e2) -> 
        (match eval_helper env e1, eval_helper env e2 with
           | V.Predicate pr1, V.Predicate pr2 -> V.Predicate (NKT.And (pr1, pr2))
           | _ -> raise 
                    (Eval_error 
                       (sprintf "%s: Mismatched types to \"&&\"" (string_of_pos p))))

    | Or (p, e1, e2) -> 
        (match eval_helper env e1, eval_helper env e2 with
           | V.Predicate pr1, V.Predicate pr2 -> V.Predicate (NKT.Or (pr1, pr2))
           | _ -> raise 
                    (Eval_error 
                      (sprintf "%s: Mismatched types to \"||\"" (string_of_pos p))))

    | Neg (p, e') -> 
        (match eval_helper env e' with
          | V.Predicate pr -> V.Predicate (NKT.Neg pr)
          | _ -> raise 
                   (Eval_error 
                     (sprintf "%s: Mismatched types to \"!\"" (string_of_pos p))))

    | Header (_, h) -> V.Header h

    | HeaderVal (_, hv) -> V.HeaderVal hv

    | TypeIs (_, e, _) -> eval_helper env e



let eval (e : exp) : NKT.policy =
  to_pol (eval_helper empty_env e)