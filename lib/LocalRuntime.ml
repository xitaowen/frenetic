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
