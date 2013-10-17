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
  | Filter of exp
  | True
  | False
  | Test of exp * exp
  | And of exp * exp
  | Or of exp * exp
  | Neg of exp
  | Header of header
  | HeaderVal of header_val

let eval (e : exp) = failwith "eval NYI"