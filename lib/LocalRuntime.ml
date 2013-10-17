module type REALIZABLE_HEADERS = sig
  include Semantics.HEADERS

  type r_header

  val realize_header : header -> r_header option
end

module type S = sig

  type policy
  type impl
  type flowTable
  type sw

  val compile : policy -> impl
  val decompile : impl -> policy

  val to_table : sw -> impl -> flowTable
end

exception Unrealizable of string list

module MakeSDN
  (Headers : REALIZABLE_HEADERS with type r_header = SDN_Headers.header
                                 and type value = SDN_Headers.value)
  (Syntax : Semantics.S with type header = Headers.header
                         and type header_val = Headers.value
                         and type payload = Headers.payload) : S = struct

  module Compiler = LocalCompiler.Make (Headers) (Syntax)

  module Local = Compiler.Local
  module Action = Compiler.Action
  module Pattern = Compiler.Pattern
  module Atom = Compiler.Atom

  module RHMap = Map.Make(struct
    type t = SDN_Headers.header
    let compare = SDN_Headers.compare_header
  end)

  type policy = Syntax.policy
  type impl = Local.t
  type flowTable = SDN_Types.flowTable
  type sw = SDN_Types.switchId

  let compile (pol:policy) : impl =
    Local.of_policy pol

  let decompile (p:impl) : policy =
    Local.to_netkat p

  (*
   * Take a HeaderMap and turn it into a a Realizable HeaderMap, collecting the
   * failed header translations and associated values in the process
   *)
  let convert (m : Headers.value Syntax.HeaderMap.t)
    : (Syntax.HeaderMap.key * Headers.value) list * Headers.value RHMap.t =
    let translate k v (bs, m) =
      match Headers.realize_header k with
        | None    -> ((k, v)::bs, m)
        | Some k' -> (bs, RHMap.add k' v m)
      in Syntax.HeaderMap.fold translate m ([], RHMap.empty)

  let to_action (a:Action.t) : SDN_Types.seq =
    let (bad, a') = convert a in
    if not (RHMap.mem (SDN_Headers.Header SDN_Types.InPort) a') then
      []
    else
      let port = RHMap.find (SDN_Headers.Header SDN_Types.InPort) a' in
      let mods = RHMap.remove (SDN_Headers.Header SDN_Types.InPort) a' in
      let mk_mod h v act =
        match h with
          | SDN_Headers.Switch -> raise (Invalid_argument "Action.to_action got switch update")
          | SDN_Headers.Header h' ->  (SDN_Types.SetField (h', v)) :: act in
      RHMap.fold mk_mod mods [SDN_Types.OutputPort port]

  let set_to_action (s:Action.Set.t) : SDN_Types.par =
    let f a par = (to_action a)::par in
    Action.Set.fold f s []

  let bad_pair_to_string (k, v) =
    (Headers.header_to_string k) ^ ": " ^ (Headers.value_to_string v)

  let to_pattern (sw : sw) (x:Pattern.t) : SDN_Types.pattern option =
    let (bad, x') = convert x in
    if List.exists (fun _ -> true) bad
      then raise (Unrealizable (List.map bad_pair_to_string bad))
      else ();
    let f (h : NetKAT_Types.header) (v : NetKAT_Types.header_val) (pat : SDN_Types.pattern) =
      match h with
        | SDN_Headers.Switch -> pat (* already tested for this *)
        | SDN_Headers.Header h' -> SDN_Types.FieldMap.add h' v pat in
    if RHMap.mem SDN_Headers.Switch x' &&
       RHMap.find SDN_Headers.Switch x' <> sw then
      None
    else
      Some (RHMap.fold f x' SDN_Types.FieldMap.empty)

  let simpl_flow (p : SDN_Types.pattern) (a : SDN_Types.group) : SDN_Types.flow = {
    SDN_Types.pattern = p;
    SDN_Types.action = a;
    SDN_Types.cookie = 0L;
    SDN_Types.idle_timeout = SDN_Types.Permanent;
    SDN_Types.hard_timeout = SDN_Types.Permanent
  }

  (* Prunes out rules that apply to other switches. *)
  let to_table (sw:sw) (p:impl) : flowTable =
    let add_flow x s l =
      match to_pattern sw x with
        | None -> l
        | Some pat -> simpl_flow pat [set_to_action s] :: l in
    let rec loop (p:impl) acc cover =
      if Atom.Map.is_empty p then
        acc
      else
        let r,s = Atom.Map.min_binding p in
        let (xs,x) = r in
        let p' = Atom.Map.remove r p in
        let ys = Pattern.Set.diff xs cover in
        let acc' = Pattern.Set.fold (fun x acc -> add_flow x Action.Set.empty acc) ys acc in
        let acc'' = add_flow x s acc' in
        let cover' = Pattern.Set.add x (Pattern.Set.union xs cover) in
        loop p' acc'' cover' in
    List.rev (loop p [] Pattern.Set.empty)

end
