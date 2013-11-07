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
  val to_table : sw -> impl -> flowTable
end

exception Unrealizable of string list

module MakeSDN
  (Headers : REALIZABLE_HEADERS with type r_header = SDN_Headers.header
                                 and type value = SDN_Headers.value)
  (Syntax : Semantics.S with type header = Headers.header
                         and type header_val = Headers.value
                         and type payload = Headers.payload) : S
    with type policy = Syntax.policy
     and type flowTable = SDN_Types.flowTable
     and type sw = SDN_Types.switchId = struct

  module Compiler = LocalCompiler.Make (Headers) (Syntax)

  module Local = Compiler.Local
  module Action = Compiler.Action
  module Pattern = Compiler.Pattern
  module Atom = Compiler.Atom

  type policy = Syntax.policy
  type flowTable = SDN_Types.flowTable
  type sw = SDN_Types.switchId

  module SwitchMap = Map.Make(struct
    type t = sw
    let compare = Pervasives.compare
  end)

  type impl = flowTable * flowTable SwitchMap.t

  module VSet = Set.Make(struct
    type t = Headers.value
    let compare = Pervasives.compare
  end)

  module VHMap = Syntax.HeaderMap
  module RHMap = Map.Make(struct
    type t = SDN_Headers.header
    let compare = SDN_Headers.compare_header
  end)

  let bad_pair_to_string (k, v) =
    (Headers.header_to_string k) ^ ": " ^ (Headers.value_to_string v)

  (*
   * Take a HeaderMap and turn it into a a Realizable HeaderMap, collecting the
   * failed header translations and associated values in the process
   *)
  let convert hmap vmap m : Headers.value RHMap.t =
    let translate k v m = RHMap.add (hmap k) (vmap k v) m in
    Syntax.HeaderMap.fold translate m RHMap.empty

  let to_action hmap vmap (a:Action.t) : SDN_Types.seq =
    let a' = convert hmap vmap a in
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

  let set_to_action hmap vmap (s:Action.Set.t) : SDN_Types.par =
    let f a par = (to_action hmap vmap a)::par in
    Action.Set.fold f s []

  let to_pattern x : (sw option) * SDN_Types.pattern =
    let f (h : NetKAT_Types.header) (v : NetKAT_Types.header_val) (pat : SDN_Types.pattern) =
      match h with
        | SDN_Headers.Switch -> pat (* already tested for this *)
        | SDN_Headers.Header h' -> SDN_Types.FieldMap.add h' v pat in
    let sw = if RHMap.mem SDN_Headers.Switch x
                then Some (RHMap.find SDN_Headers.Switch x)
                else None in
    (sw, RHMap.fold f x SDN_Types.FieldMap.empty)

  let simpl_flow (p : SDN_Types.pattern) (a : SDN_Types.group) : SDN_Types.flow = {
    SDN_Types.pattern = p;
    SDN_Types.action = a;
    SDN_Types.cookie = 0L;
    SDN_Types.idle_timeout = SDN_Types.Permanent;
    SDN_Types.hard_timeout = SDN_Types.Permanent
  }

  let header_usage (p:Local.t) : (VSet.t VHMap.t) * (VSet.t RHMap.t) =
    let update find add k v s =
      try add k (VSet.union (find k s) v) s with Not_found -> add k v s in

    let pattern_usage (p:Pattern.t) =
      let f h v (vmap, rmap) =
        match Headers.realize_header h with
          | None    -> (update VHMap.find VHMap.add h (VSet.singleton v) vmap, rmap)
          | Some h' -> (vmap, update RHMap.find RHMap.add h' (VSet.singleton v) rmap) in
      Syntax.HeaderMap.fold f p in

    let atom_usage (pat : Atom.t) (acts : Action.Set.t) ms =
      let (negs, pos) = pat in
      let ms' = pattern_usage pos ms in
      let ms'' = Action.Set.fold (fun a acc -> pattern_usage a acc) acts ms' in
      Pattern.Set.fold (fun p acc -> pattern_usage p acc) negs ms'' in

    Atom.Map.fold atom_usage p (VHMap.empty, RHMap.empty)

  let devirtualize (maps : (VSet.t VHMap.t) * (VSet.t RHMap.t)) :
      (Headers.header -> Headers.r_header) * (Headers.header -> Headers.value -> Headers.value) =
    let (vmap, rmap) = maps in
    if not (VHMap.is_empty vmap)
      then raise (Unrealizable []); (* TODO(seliopou): Provide useful list *)
    let f h =
      match Headers.realize_header h with
       | Some h' -> h'
       | None -> raise (Unrealizable []) in (* TODO(seliopou): Provide useful list *)
    let g h v = v in
      (f, g)

  let realize
      (hmap : Headers.header -> Headers.r_header)
      (vmap : Headers.header -> Headers.value -> Headers.value)
      (p : Local.t) : impl =

    let update_switch sw v switches =
      try SwitchMap.add sw (v :: (SwitchMap.find sw switches)) switches
        with Not_found -> SwitchMap.add sw [v] switches in
   
    let add_flow x s (single, switches) =
      let x' = convert hmap vmap x in
      let (sw, pat) = to_pattern x' in
      let flow = simpl_flow pat [set_to_action hmap vmap s] in
      match sw with
        | None -> (flow :: single, SwitchMap.map (fun l -> flow :: l) switches)
        | Some sw' -> (single, update_switch sw' flow switches) in

    let rec loop (p:Local.t) acc cover : impl =
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
    let (single, switches) = loop p ([], SwitchMap.empty) Pattern.Set.empty in
      (List.rev single, SwitchMap.map List.rev switches)

  let compile (pol:policy) : impl =
    let local = Local.of_policy pol in
    let (hmap, vmap) = devirtualize (header_usage local) in
    realize hmap vmap local

  let to_table (sw:sw) ((f, m):impl) : flowTable =
    if SwitchMap.mem sw m
      then SwitchMap.find sw m
      else f
end

module SDNRuntime = MakeSDN (struct
  include SDN_Headers
  type r_header = SDN_Headers.header

  let realize_header h = Some h
end) (NetKAT_Types)
