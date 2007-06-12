%{

  open Constraint.Raw

%}

%token<string> ID
%token LET COLON FORALL LBRACK RBRACK IN EXISTS DOT INST LPAREN RPAREN ARROW
%token INT STRING EQ AND EOF TRUE FALSE DUMP

%start input
%type<Constraint.Raw.ty_constraint> input

%nonassoc IN
%nonassoc DOT
%nonassoc AND
%nonassoc ARROW

%%


input: c=tconstraint EOF
{
  c
}

tconstraint: 
 c1=tconstraint AND c2=tconstraint
{
  CAnd (c1, c2)
}
| LET x=ID COLON FORALL xs=ID+ LBRACK c1=tconstraint RBRACK t=typ 
  IN c2=tconstraint
{
  CLet (x, (xs, c1, t), c2)
}
| LET x=ID COLON t=typ IN c=tconstraint 
{
  CLet (x, ([], CTrue, t), c)
}
| EXISTS xs=ID+ DOT c=tconstraint
{
  CExists (xs, c)
}
| t1=typ EQ t2=typ
{
  CEq (t1, t2)
}
| x=ID INST t=typ
{
  CInst (x, t)
}
| LPAREN c=tconstraint RPAREN
{
  c
}
| TRUE
{
  CTrue
}
| FALSE
{
  CFalse
}
| DUMP
{
  CDump
}

typ: x=ID
{
  CTyApp (x, [])
}
| t1=typ ARROW t2=typ
{
  CTyArrow (t1, t2)
}
| INT
{
  CTyConstant CTyInt
}
| STRING
{
  CTyConstant CTyString
}
| LPAREN t=typ RPAREN
{
  t
}
