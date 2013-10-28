module TS = TemplateSyntax

exception Type_error of string




(* TODO : Make a functor module out of environment, that could map id to value or id to type *)
module Env = Map.Make (String)

type env = TS.typ Env.t


let env_add    = Env.add
let env_lookup = Env.find
let empty_env  = Env.empty

(* TODO : use pos to indicate appropriate error *)

let rec synth (env : env) (e : TS.exp) : TS.typ =
  match e with 
    | TS.Id   (p, x) -> 
      (try 
         env_lookup x env 
       with Not_found -> raise (Type_error ("Unbound identifier" ^ x)))

    | TS.Let  (p, x, with_e, in_e) -> 
        let with_e_t = synth env with_e in
        let env' = env_add x with_e_t env in
        synth env' in_e

    | TS.Fun  (p, param_list, body) -> 
        raise (Type_error "Cannot synthesize type of function")

    | TS.App  (p, f, arg_list) -> 
        (* 
         * synthesize the type of f and see that it is a function
         * "check" that type of each element in arg_list matches corresponding
         * function param type
         * if it does then synth should return type of function body
         * else it should throw a type error
         *)

        (match synth env f with
          | TS.TFun (t_param_list, t_body) -> 
              (try
                 let open List in
                 let f (e' : TS.exp) 
                       (t' : TS.typ) 
                       (acc : bool) = (check env e' t') && acc in
                 let res = fold_right2 f arg_list t_param_list true in
                 if res 
                 then t_body
                 else raise (Type_error "Wrong type of argument applied to function")
              with Invalid_argument _ -> raise (Type_error "Mismatch in lengths of list"))

              
          | _ -> raise (Type_error "Expected a function"))
        
      
    | TS.If  (p, e_cond, e_true, e_false) ->
        if check env e_cond  TS.TPred &&
           check env e_true  TS.TPol  && 
           check env e_false TS.TPol
        then TS.TPol
        else raise (Type_error "Type error in if")

    | TS.Par (p, e1, e2)
    | TS.Seq (p, e1, e2) ->
        if check env e1 TS.TPol &&
           check env e2 TS.TPol
        then TS.TPol
        else raise (Type_error "Type error in Par/Seq") 

    | TS.Mod  (p, e1, e2) ->
        failwith "Take width of hdr and hdrval into account"

    | TS.Filter  (p, e) ->
        if check env e TS.TPred
        then TS.TPol
        else raise (Type_error "Expected a predicate with Filter")

    | TS.True  (p)
    | TS.False  (p) ->
        TS.TPred

    | TS.Test  (p, e1, e2) ->
        failwith "Take width of hdr and hdrval into account"

    | TS.And  (p, e1, e2)
    | TS.Or  (p, e1, e2) ->
        if check env e1 TS.TPred &&
           check env e2 TS.TPred
        then TS.TPred
        else raise (Type_error "And/Or expects to work on predicates")

    | TS.Neg  (p, e) ->
        if check env e TS.TPred
        then TS.TPred
        else raise (Type_error "Neg expects to work on predicates")

    | TS.Header  (p, hdr) ->
        failwith "Take width of hdr and hdrval into account"

    | TS.HeaderVal  (p, hdr_val) ->
        failwith "Take width of hdr and hdrval into account"

    | TS.TypeIs  (p, e, t) ->
        if check env e t
        then t
        else raise (Type_error "Incorrect type annotation")


and 

check (env : env) (e : TS.exp) (t : TS.typ) : bool =

  match e with

  | TS.Id (p, x) -> 
      (try
         t = env_lookup x env
       with Not_found -> raise (Type_error ("Unbound Identifier")))

  | TS.Let (p, x, with_e, in_e) ->
      let env' = env_add x (synth env with_e) env
      in check env' in_e t
      
  | TS.Fun (p, params, body) -> 
      (match t with
         | TS.TFun (t_params, t_body) ->
             (*
              * 1. Augment environment with new types involved in t_param_list
              * 2. check if type of body matches with 't' *)

             (try
                let open List in 
                let env' = fold_right2 env_add params t_params env in
                check env' body t_body
              with Invalid_argument _ -> raise (Type_error "mismatched number of arguments"))

         | _ -> false)

  | TS.App (p, f, args) ->
      let open List in
      let t_args = map (synth env) args in
      check env f (TS.TFun (t_args, t))
      
      
  | TS.If (p, e_cond, e_true, e_false) ->
      check env e_cond  TS.TPred &&
      check env e_true  TS.TPol  &&
      check env e_false TS.TPol

  | TS.Par (p, e1, e2)
  | TS.Seq (p, e1, e2) ->
      check env e1 TS.TPol &&
      check env e2 TS.TPol

  | TS.Mod (p, e1, e2) ->
        failwith "Take width of hdr and hdrval into account"

  | TS.Filter (p, e) ->
      check env e TS.TPred

  | TS.True (p)
  | TS.False (p) ->
      (match t with
        | TS.TPred -> true
        | _ -> false)

  | TS.Test (p, e1, e2) ->
        failwith "Take width of hdr and hdrval into account"

  | TS.And (p, e1, e2)
  | TS.Or  (p, e1, e2) ->
      check env e1 TS.TPred &&
      check env e2 TS.TPred

  | TS.Neg (p, e) ->
      check env e TS.TPred

  | TS.Header (p, e) ->
        failwith "Take width of hdr and hdrval into account"

  | TS.HeaderVal (p, e) ->
        failwith "Take width of hdr and hdrval into account"

  | TS.TypeIs (p, e, t) ->
      check env e t

