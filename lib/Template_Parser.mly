%{
  open SDN_Headers
  open TemplateSyntax

  (* Ethernet frame types *)
  let arp : int64  = Int64.of_int 0x806
  let ip  : int64  = Int64.of_int 0x800

  (* Ip protocol types *)
  let icmp : int64 = Int64.of_int 0x01
  let tcp  : int64 = Int64.of_int 0x06
  let udp  : int64 = Int64.of_int 0x11

  (* hack for now *)
  let vlan_none : int64 = Int64.minus_one
%}

%token LPAREN
%token RPAREN
%token COLON
%token TPOL
%token TPRED
%token TRUE
%token FALSE
%token NONE
%token EQUALS
%token SWITCH
%token VLAN
%token SRCMAC
%token DSTMAC
%token SRCIP
%token DSTIP
%token TCPSRCPORT
%token TCPDSTPORT
%token PORT
%token <Int64.t> INT64
%token <Int64.t> MACADDR
%token <Int64.t> IPADDR
%token IF
%token THEN
%token ELSE
%token SEMI
%token PLUS
%token COMMA
%token LET
%token IN
%token FRAMETYPE
%token PROTOCOLTYPE
%token ARP
%token IP
%token ICMP
%token TCP
%token UDP
%token <string> IDENT
%token EOF
%token BANG
%token AMPAMP
%token PIPEPIPE
%token FILTER
%token COLONEQ
%token FUN
%token RARROW




%start program


%type <TemplateSyntax.exp> program

%%

ident_list :
  |                        { [] }
  | IDENT                  { [$1] }
  | IDENT COMMA ident_list { $1 :: $3 }


atom_typ :
  | TPRED             { TPred }
  | TPOL              { TPol  }
  | LPAREN typ RPAREN { $2 }



typ :
  | atom_typ { $1 }
  | atom_typ RARROW atom_typ { TFun ([$1], $3) }

  /* | THEADER { THdr (some width) }    */
  /* | THEADERVAL { TInt (some width) } */



arg_type_list :
  |                                     { [] }
  | IDENT COLON typ                     { [($1, $3)] }
  | IDENT COLON typ COMMA arg_type_list { ($1, $3) :: $5 }

header :
  | SWITCH       { SDN_Headers.Switch                      }
  | PORT         { SDN_Headers.Header SDN_Types.InPort     }
  | TCPSRCPORT   { SDN_Headers.Header SDN_Types.TCPSrcPort }
  | TCPDSTPORT   { SDN_Headers.Header SDN_Types.TCPDstPort }
  | SRCMAC       { SDN_Headers.Header SDN_Types.EthSrc     }
  | DSTMAC       { SDN_Headers.Header SDN_Types.EthDst     }
  | VLAN         { SDN_Headers.Header SDN_Types.Vlan       }
  | SRCIP        { SDN_Headers.Header SDN_Types.IP4Src     }
  | DSTIP        { SDN_Headers.Header SDN_Types.IP4Dst     }
  | FRAMETYPE    { SDN_Headers.Header SDN_Types.EthType    }
  | PROTOCOLTYPE { SDN_Headers.Header SDN_Types.IPProto    }


header_val :
  | INT64   { $1        }
  | MACADDR { $1        }
  | NONE    { vlan_none }
  | IPADDR  { $1        }
  | ARP     { arp       }
  | IP      { ip        }
  | ICMP    { icmp      }
  | TCP     { tcp       }
  | UDP     { udp       }

header_or_id :
  | header { Header (symbol_start_pos (), $1) }
  | IDENT  { Id (symbol_start_pos (), $1) }

header_val_or_id :
  | header_val { HeaderVal (symbol_start_pos (), VInt.Int64 $1) }
  | IDENT      { Id (symbol_start_pos (), $1) }

atom_pred :
  | TRUE                            { True (symbol_start_pos ()) }
  | FALSE                           { False (symbol_start_pos ()) }
  | LPAREN pred RPAREN                   { $2 }
  | IDENT                                { Id (symbol_start_pos (), $1) }
  | header_or_id EQUALS header_val_or_id { Test (symbol_start_pos (), $1, $3) }
  | BANG atom_pred                       { Neg (symbol_start_pos (), $2) }

and_pred :
  | atom_pred                 { $1 }
  | and_pred AMPAMP atom_pred { And (symbol_start_pos (), $1, $3) }

or_pred :
  | and_pred                  { $1 }
  | or_pred PIPEPIPE and_pred { Or (symbol_start_pos (), $1, $3) }

pred :
  or_pred { $1 }

atom_exp :
  | LPAREN exp RPAREN               { $2 }
  | header                          { Header (symbol_start_pos (), $1) }
  | header_val                      { HeaderVal (symbol_start_pos (), VInt.Int64 $1) }
  | IDENT                           { Id (symbol_start_pos (), $1) }
  | atom_exp LPAREN exp_list RPAREN { App (symbol_start_pos (), $1, $3) }
  | FILTER pred                     { Filter (symbol_start_pos (), $2) }

mod_exp :
  | atom_exp                  { $1 }
  | atom_exp COLONEQ atom_exp { Mod (symbol_start_pos (), $1, $3) }  

seq_exp :
  | mod_exp              { $1 }
  | seq_exp SEMI mod_exp { Seq (symbol_start_pos (), $1, $3) }

par_exp :
  | seq_exp               { $1 }
  | par_exp PLUS seq_exp  { Par (symbol_start_pos (), $1, $3) }

exp_list :
  |                    { [] }
  | exp                { [$1] }
  | exp COMMA exp_list { $1 :: $3 }

exp :
  | par_exp                      { $1 }
  | LET IDENT EQUALS exp IN exp  { Let (symbol_start_pos (), $2, $4, $6) }
  | IF exp THEN exp ELSE exp     { If (symbol_start_pos (), $2, $4, $6) }
  | FUN ident_list RARROW exp    { Fun (symbol_start_pos (), $2, $4) }

  | FUN arg_type_list COLON typ EQUALS exp {
                                             let (id_list, id_type_list) = List.split $2 in
                                             TypeIs (symbol_start_pos (), 
                                                     Fun (symbol_start_pos (), id_list, $6),
                                                     TFun (id_type_list, $4))
                                           }
                                           

program : 
  | exp EOF  { $1 }

%%
