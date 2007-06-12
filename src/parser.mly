(* $Id$ *)

%{

  open Positions
  open Syntax.Raw
  open Level

  let mkPos startpos endpos =
    Positions.lex_join startpos endpos

  let mkFun annot params t = 
    List.fold_right(fun b t -> ELambda (annot, (b, t))) params t

%}

(* Annotated token. *)
%token<string> ID LID TID
%token<int> INT
%token<string> STRING
%token<string> BINOP

(* Keywords. *)
%token VAL VALREC LET TYPE LETREC MATCH IN END WITH OF LAMBDA TYINT TYSTRING

(* Punctuation. *)
%token EQ LPAREN RPAREN COLON PIPE DOT EOF

(* Symbols. *)
%token ARROW STAR

(* We are parsing [program]. *)
%start<Syntax.Raw.program> program

%nonassoc LETREC LET MATCH BINOP
%nonassoc LAMBDA DOT IN
%nonassoc LID ID INT STRING 
%nonassoc LPAREN
%left app_prec

%%

program: EOF
{
  EmptyProgram
}
| td=toplevel_definition p=program
{
  let pos = mkPos $startpos $endpos in
    NewDefinition (pos, (td, p))
}

%inline toplevel_definition: d=type_definition | d=val_definition
{
  d
}

%inline type_definition: 
  TYPE ty=ID params=TID* EQ d=ty_def END
{
  TypeDefinition (ty, params, d)
}

%inline val_definition: 
  VAL d=term_def 
{
  let (x, d) = d in
    ValDefinition (x, d)
}
| VALREC d=term_def 
{
  let (x, d) = d in
    RecDefinitions (x, d)
}

term_def: x=identifier params=binder* EQ t=term
{
  let pos = mkPos $startpos $endpos in
    (x, mkFun pos params t)
}

identifier: x=ID | LPAREN x=BINOP RPAREN
{ 
  x 
}

term:
  x=LID | x=ID
{
  let pos = mkPos $startpos $endpos in
    EVar (pos, x)
}
| t1=term t2=term %prec app_prec
{
  let pos = mkPos $startpos $endpos in
    EApp (pos, PrefixApp, t1, t2)
}
| LAMBDA b=binder DOT t=term
{
  let pos = mkPos $startpos $endpos in
    ELambda (pos, (b, t))
}
| LET d=term_def IN t=term
{
  let pos = mkPos $startpos $endpos in
  let (x, t') = d in
    ELet (pos, (x, t', t))
}
| LETREC d=term_def IN t=term
{
  let pos = mkPos $startpos $endpos in
  let (x, t') = d in
    ELetRec (pos, (x, t', t))
}
| c=constant
{
  let pos = mkPos $startpos $endpos in
    EConstant (pos, c)
}
| t1=term c=BINOP t2=term
{
  let pos = mkPos $startpos $endpos in
  let op = c in
    EApp (pos, PrefixApp, EApp (pos, InfixApp 0, EVar (pos, op), t1), t2)
}
| LPAREN t=term RPAREN
{
  t
}
| MATCH t=term WITH cs=nonempty_list(preceded(PIPE, clause)) END
{
  let pos = mkPos $startpos $endpos in
    EMatch (pos, t, cs)
}
| LPAREN t=term COLON ty=ty RPAREN
{
  let pos = mkPos $startpos $endpos in
    EAnnot (pos, t, ty)
}

binder: x=ID
{
  Bind x
}
| LPAREN x=ID COLON ty=ty RPAREN
{
  AnnotatedBind (x, ty)
}
| LPAREN b=binop RPAREN 
{
  Bind b
}

binop: 
  b=BINOP
{ 
  b
}
| STAR
{
  "*"
}

clause: p=pattern ARROW t=term
{
  let pos = mkPos $startpos $endpos in
    (pos, p, t)
}

pattern: 
  p=pattern0
{
  p
}
| k=LID ps=pattern0+ 
{
  let pos = mkPos $startpos $endpos in
    PDataCon (pos, k, ps)
}

pattern0:
  x=ID
{ 
  let pos = mkPos $startpos $endpos in
    PVar (pos, x)
}
| k=LID 
{
  let pos = mkPos $startpos $endpos in
    PDataCon (pos, k, [])  
}
| LPAREN p=pattern RPAREN
{
  p
}

ty: 
  ty=ty0
{
  ty
}
| t1=ty0 ARROW t2=ty
{
  let pos = mkPos $startpos $endpos in
    TyArrow (pos, t1, t2)
}
| x=ID args=ty0+
{
  let pos = mkPos $startpos $endpos in
    TyApp (pos, x, args)
}

ty0:
  c=tyconstant
{
  let pos = mkPos $startpos $endpos in
    TyConstant (pos, c)
}
| LPAREN ty=ty RPAREN
{
  ty
}
| x=TID
{
  let pos = mkPos $startpos $endpos in
    TyVar (pos, x)
}
| x=ID 
{
  let pos = mkPos $startpos $endpos in
    TyApp (pos, x, [])
}

ty_def: 
  i=adt_definition
{
  AlgebraicDataType i
}

adt_definition:
  PIPE? dts=separated_list(PIPE, dt_definition)
{
  dts
}

dt_definition:
  k=LID OF ty=separated_nonempty_list(STAR, ty)
{
  DataType (k, ty)
}
| k=LID
{
  DataType (k, [])
}

constant:
  x=INT
{
  Int x
}
| x=STRING
{
  String x
}

tyconstant:
  TYINT
{
  TyInt
}
| TYSTRING
{
  TyString
}
