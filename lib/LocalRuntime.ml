module type S = sig

  type policy
  type impl
  type flowTable
  type sw

  val compile : policy -> impl
  val decompile : impl -> policy

  val to_table : sw -> impl -> flowTable
end
