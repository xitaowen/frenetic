module TS = TemplateSyntax

exception Type_error of string


module Env = Map.Make (String)

type env = TS.typ Env.t

module NKT = NetKAT_Types


let env_add    = Env.add
let env_lookup = Env.find
let empty_env  = Env.empty

(* TODO : use pos to indicate appropriate error *)



let rec get_msb_loc (x : Int64.t) : int =
  if x <= Int64.zero then 0
  else 1 + get_msb_loc (Int64.shift_right_logical x 1)

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

    | TS.Filter  (p, e) ->
        if check env e TS.TPred
        then TS.TPol
        else raise (Type_error "Expected a predicate with Filter")

    | TS.True  (p)
    | TS.False  (p) ->
        TS.TPred

    | TS.Mod  (p, e1, e2) ->
        let t_e1 = synth env e1 in
        let t_e2 = synth env e2 in
        (match t_e1, t_e2 with
           | TS.THdr w1, TS.TInt w2 -> if w1 >= w2
                                       then TS.TPol
                                       else raise (Type_error "Value is greater than what header can accomodate")

           | _ -> raise (Type_error "Mismatched type"))

    | TS.Test  (p, e1, e2) ->
        let t_e1 = synth env e1 in
        let t_e2 = synth env e2 in
        (match t_e1, t_e2 with
           | TS.THdr w1, TS.TInt w2 -> if w1 >= w2
                                       then TS.TPred
                                       else raise (Type_error "Value is greater than what header can accomodate")

           | _ -> raise (Type_error "Mismatched type"))


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
        let module SDNH = SDN_Headers in
        (match hdr with
           | SDNH.Header (h) -> 
               let module SDNT = SDN_Types in
               (match h with
                 | SDNT.IPProto
                 | SDNT.EthType -> TS.THdr (8)

                 | SDNT.EthSrc
                 | SDNT.EthDst -> TS.THdr (48)

                 | SDNT.Vlan
                 | SDNT.VlanPcp -> TS.THdr (16)

                 | SDNT.IP4Src
                 | SDNT.IP4Dst -> TS.THdr (32)

                 | SDNT.InPort
                 | SDNT.TCPSrcPort
                 | SDNT.TCPDstPort -> TS.THdr (16))

           | SDNH.Switch  -> TS.THdr (64))
        
        
    | TS.HeaderVal  (p, hdr_val) ->
        TS.TInt (get_msb_loc (VInt.get_int64 hdr_val))
        

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
      (check env e_cond  TS.TPred) &&
      (check env e_true  TS.TPol)  &&
      (check env e_false TS.TPol)  &&
      (t = TS.TPol)

  | TS.Par (p, e1, e2)
  | TS.Seq (p, e1, e2) ->
      check env e1 TS.TPol &&
      check env e2 TS.TPol &&
      t = TS.TPol


  | TS.Filter (p, e) ->
      check env e TS.TPred && t = TS.TPred

  | TS.True (p)
  | TS.False (p) -> t = TS.TPred

  | TS.Mod (p, e1, e2) ->
      (TS.TPol = synth env e) && (t = TS.TPol)

  | TS.Test (p, e1, e2) ->
      (TS.TPred = synth env e) && (t = TS.TPred)

  | TS.And (p, e1, e2)
  | TS.Or  (p, e1, e2) ->
      check env e1 TS.TPred &&
      check env e2 TS.TPred &&
      t = TS.TPred

  | TS.Neg (p, e) ->
      check env e TS.TPred && t = TS.TPred

  | TS.Header (p, h) ->
      (match synth env e, t with
         | TS.THdr w1, TS.THdr w2 -> w1 = w2
         | _ -> false)

  | TS.HeaderVal (p, hv) ->
      (match synth env e, t with
         | TS.TInt w1, TS.TInt w2 -> w1 = w2
         | _ -> false)

  | TS.TypeIs (p, e, t') ->
      check env e t' && t = t' 


let type_check (e : TS.exp) (t : TS.typ) : bool =
  check empty_env e t
