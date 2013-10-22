type pos = Lexing.position

type id = string

type header = NetKAT_Types.header

type header_val = NetKAT_Types.header_val

type exp =
  | Id of pos * id
  | Let of pos * id * exp * exp
  | Fun of pos * id list * exp
  | App of pos * exp * exp list
  | If of pos * exp * exp * exp
  | Par of pos * exp * exp
  | Seq of pos * exp * exp
  | Mod of pos * exp * exp
  | Filter of pos * exp
  | True of pos 
  | False of pos
  | Test of pos * exp * exp
  | And of pos * exp * exp
  | Or of pos * exp * exp
  | Neg of pos * exp
  | Header of pos * header
  | HeaderVal of pos * header_val

val eval : exp -> NetKAT_Types.policy
