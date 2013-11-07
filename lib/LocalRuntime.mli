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

module MakeSDN
  (Headers : REALIZABLE_HEADERS with type r_header = SDN_Headers.header
                                 and type value = SDN_Headers.value)
  (Syntax : Semantics.S with type header = Headers.header
                         and type header_val = Headers.value
                         and type payload = Headers.payload) : S
    with type policy = Syntax.policy
     and type flowTable = SDN_Types.flowTable
     and type sw = SDN_Types.switchId

module SDNRuntime : S
  with type policy = NetKAT_Types.policy
   and type flowTable = SDN_Types.flowTable
   and type sw = SDN_Types.switchId
