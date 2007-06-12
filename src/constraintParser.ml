  

  open Constraint.Raw

exception Error

type token = 
  | TRUE
  | STRING
  | RPAREN
  | RBRACK
  | LPAREN
  | LET
  | LBRACK
  | INT
  | INST
  | IN
  | ID of (string)
  | FORALL
  | FALSE
  | EXISTS
  | EQ
  | EOF
  | DUMP
  | DOT
  | COLON
  | ARROW
  | AND

and _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  mutable _menhir_token: token;
  mutable _menhir_startp: Lexing.position;
  mutable _menhir_endp: Lexing.position;
  mutable _menhir_shifted: int
}

and _menhir_state = 
  | MenhirState0
  | MenhirState3
  | MenhirState6
  | MenhirState7
  | MenhirState12
  | MenhirState14
  | MenhirState15
  | MenhirState18
  | MenhirState20
  | MenhirState23
  | MenhirState25
  | MenhirState28
  | MenhirState31
  | MenhirState34
  | MenhirState36
  | MenhirState39

let _eRR =
  Error

let rec _menhir_run28 : _menhir_env -> 'ttv_tail * _menhir_state * (Constraint.Raw.ty) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | INT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | STRING ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | _ ->
        assert (_menhir_env._menhir_shifted >= 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_error28 _menhir_env _menhir_stack

and _menhir_run11 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (Constraint.Raw.ty) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let ((_menhir_stack, _menhir_s), _, t) = _menhir_stack in
    let _v : (Constraint.Raw.ty) = (
  t
) in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run12 : _menhir_env -> 'ttv_tail * _menhir_state * (Constraint.Raw.ty) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | INT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | STRING ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | _ ->
        assert (_menhir_env._menhir_shifted >= 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_error12 _menhir_env _menhir_stack

and _menhir_error39 : _menhir_env -> (('ttv_tail * _menhir_state) * (string)) * _menhir_state * (Constraint.Raw.ty) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error36 : _menhir_env -> ((((('ttv_tail * _menhir_state) * (string)) * _menhir_state) * _menhir_state * (Constraint.Raw.ex_var list)) * _menhir_state * (Constraint.Raw.ty_constraint)) * _menhir_state * (Constraint.Raw.ty) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error31 : _menhir_env -> 'ttv_tail * _menhir_state * (Constraint.Raw.ty_constraint) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error28 : _menhir_env -> 'ttv_tail * _menhir_state * (Constraint.Raw.ty) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error25 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (Constraint.Raw.ex_var list) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error18 : _menhir_env -> ((('ttv_tail * _menhir_state) * (string)) * _menhir_state) * _menhir_state * (Constraint.Raw.ex_var list) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error12 : _menhir_env -> 'ttv_tail * _menhir_state * (Constraint.Raw.ty) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error7 : _menhir_env -> 'ttv_tail * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error15 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_nonempty_list_ID_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Constraint.Raw.ex_var list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (Constraint.Raw.ex_var list) =     ( x :: xs ) in
        _menhir_goto_nonempty_list_ID_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (_menhir_env._menhir_shifted <> (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACK ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | DUMP ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | EXISTS ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | FALSE ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | ID _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
            | INT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | LET ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | STRING ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | TRUE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | _ ->
                assert (_menhir_env._menhir_shifted >= 0);
                _menhir_env._menhir_shifted <- (-1);
                _menhir_error18 _menhir_env _menhir_stack)
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (_menhir_env._menhir_shifted <> (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | DUMP ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | EXISTS ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | FALSE ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | ID _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
            | INT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | LET ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | STRING ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | TRUE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | _ ->
                assert (_menhir_env._menhir_shifted >= 0);
                _menhir_env._menhir_shifted <- (-1);
                _menhir_error25 _menhir_env _menhir_stack)
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_error34 : _menhir_env -> (((('ttv_tail * _menhir_state) * (string)) * _menhir_state) * _menhir_state * (Constraint.Raw.ex_var list)) * _menhir_state * (Constraint.Raw.ty_constraint) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run31 : _menhir_env -> 'ttv_tail * _menhir_state * (Constraint.Raw.ty_constraint) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DUMP ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | EXISTS ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | FALSE ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | ID _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | INT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | LET ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | STRING ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | TRUE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | _ ->
        assert (_menhir_env._menhir_shifted >= 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_error31 _menhir_env _menhir_stack

and _menhir_error3 : _menhir_env -> 'ttv_tail * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error6 : _menhir_env -> ('ttv_tail * _menhir_state) * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error14 : _menhir_env -> (('ttv_tail * _menhir_state) * (string)) * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Constraint.Raw.ty) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (_menhir_env._menhir_shifted <> (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, t1), _, t2) = _menhir_stack in
        let _v : (Constraint.Raw.ty) = (
  CTyArrow (t1, t2)
) in
        _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (_menhir_env._menhir_shifted <> (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | AND | EOF | RBRACK | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, x), _, t) = _menhir_stack in
            let _v : (Constraint.Raw.ty_constraint) = (
  CInst (x, t)
) in
            _menhir_goto_tconstraint _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState0 | MenhirState39 | MenhirState36 | MenhirState18 | MenhirState31 | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (_menhir_env._menhir_shifted <> (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (_menhir_env._menhir_shifted <> (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | AND | EOF | RBRACK | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, t1), _, t2) = _menhir_stack in
            let _v : (Constraint.Raw.ty_constraint) = (
  CEq (t1, t2)
) in
            _menhir_goto_tconstraint _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (_menhir_env._menhir_shifted <> (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | DUMP ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | EXISTS ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | FALSE ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | ID _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
            | INT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | LET ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | STRING ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | TRUE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | _ ->
                assert (_menhir_env._menhir_shifted >= 0);
                _menhir_env._menhir_shifted <- (-1);
                _menhir_error36 _menhir_env _menhir_stack)
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (_menhir_env._menhir_shifted <> (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | DUMP ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState39
            | EXISTS ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState39
            | FALSE ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState39
            | ID _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
            | INT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState39
            | LET ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState39
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState39
            | STRING ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState39
            | TRUE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState39
            | _ ->
                assert (_menhir_env._menhir_shifted >= 0);
                _menhir_env._menhir_shifted <- (-1);
                _menhir_error39 _menhir_env _menhir_stack)
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (_menhir_env._menhir_shifted <> (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState0 ->
        _menhir_error0 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState3 ->
        _menhir_error3 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState6 ->
        _menhir_error6 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState7 ->
        _menhir_error7 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState12 ->
        _menhir_error12 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState14 ->
        _menhir_error14 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState15 ->
        _menhir_error15 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState18 ->
        _menhir_error18 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState20 ->
        _menhir_error20 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState23 ->
        _menhir_error23 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState25 ->
        _menhir_error25 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState28 ->
        _menhir_error28 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState31 ->
        _menhir_error31 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState34 ->
        _menhir_error34 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState36 ->
        _menhir_error36 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState39 ->
        _menhir_error39 _menhir_env (Obj.magic _menhir_stack)

and _menhir_reduce14 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, x) = _menhir_stack in
    let _v : (Constraint.Raw.ty) = (
  CTyApp (x, [])
) in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_error20 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | INT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | STRING ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | _ ->
        assert (_menhir_env._menhir_shifted >= 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_error7 _menhir_env _menhir_stack

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)

and _menhir_error23 : _menhir_env -> 'ttv_tail * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | DOT | LBRACK ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (Constraint.Raw.ex_var list) =     ( [ x ] ) in
        _menhir_goto_nonempty_list_ID_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (_menhir_env._menhir_shifted >= 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_error15 _menhir_env _menhir_stack

and _menhir_goto_tconstraint : _menhir_env -> 'ttv_tail -> _menhir_state -> (Constraint.Raw.ty_constraint) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (_menhir_env._menhir_shifted <> (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | EOF | RBRACK | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, xs), _, c) = _menhir_stack in
            let _v : (Constraint.Raw.ty_constraint) = (
  CExists (xs, c)
) in
            _menhir_goto_tconstraint _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, c1), _, c2) = _menhir_stack in
        let _v : (Constraint.Raw.ty_constraint) = (
  CAnd (c1, c2)
) in
        _menhir_goto_tconstraint _menhir_env _menhir_stack _menhir_s _v
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (_menhir_env._menhir_shifted <> (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | RBRACK ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ID _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
            | INT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | LPAREN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | STRING ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | _ ->
                assert (_menhir_env._menhir_shifted >= 0);
                _menhir_env._menhir_shifted <- (-1);
                _menhir_error34 _menhir_env _menhir_stack)
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (_menhir_env._menhir_shifted <> (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | EOF | RBRACK | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((((_menhir_stack, _menhir_s), x), _), _, xs), _, c1), _, t), _, c2) = _menhir_stack in
            let _v : (Constraint.Raw.ty_constraint) = (
  CLet (x, (xs, c1, t), c2)
) in
            _menhir_goto_tconstraint _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (_menhir_env._menhir_shifted <> (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | EOF | RBRACK | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), x), _, t), _, c) = _menhir_stack in
            let _v : (Constraint.Raw.ty_constraint) = (
  CLet (x, ([], CTrue, t), c)
) in
            _menhir_goto_tconstraint _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (_menhir_env._menhir_shifted <> (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, c) = _menhir_stack in
            let _v : (Constraint.Raw.ty_constraint) = (
  c
) in
            _menhir_goto_tconstraint _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (_menhir_env._menhir_shifted <> (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, c) = _menhir_stack in
            let _v : (Constraint.Raw.ty_constraint) = (
  c
) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = _v in
            Obj.magic _1
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_discard : _menhir_env -> token =
  fun _menhir_env ->
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = _menhir_env._menhir_lexer lexbuf in
    _menhir_env._menhir_token <- _tok;
    _menhir_env._menhir_startp <- lexbuf.Lexing.lex_start_p;
    _menhir_env._menhir_endp <- lexbuf.Lexing.lex_curr_p;
    let shifted = _menhir_env._menhir_shifted + 1 in
    if shifted >= 0 then
      _menhir_env._menhir_shifted <- shifted;
    _tok

and _menhir_error0 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Constraint.Raw.ty_constraint) = (
  CTrue
) in
    _menhir_goto_tconstraint _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Constraint.Raw.ty) = (
  CTyConstant CTyString
) in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DUMP ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | EXISTS ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | FALSE ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | ID _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | INT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | LET ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | STRING ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | TRUE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | _ ->
        assert (_menhir_env._menhir_shifted >= 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_error3 _menhir_env _menhir_stack

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FORALL ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState6 in
                let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | ID _v ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
                | _ ->
                    assert (_menhir_env._menhir_shifted >= 0);
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_error14 _menhir_env _menhir_stack)
            | ID _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
            | INT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | LPAREN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | STRING ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | _ ->
                assert (_menhir_env._menhir_shifted >= 0);
                _menhir_env._menhir_shifted <- (-1);
                _menhir_error6 _menhir_env _menhir_stack)
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (_menhir_env._menhir_shifted >= 0);
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Constraint.Raw.ty) = (
  CTyConstant CTyInt
) in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | INST ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | ID _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
        | INT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | LPAREN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | STRING ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_error20 _menhir_env _menhir_stack)
    | ARROW | EQ | RPAREN ->
        _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (_menhir_env._menhir_shifted >= 0);
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Constraint.Raw.ty_constraint) = (
  CFalse
) in
    _menhir_goto_tconstraint _menhir_env _menhir_stack _menhir_s _v

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | _ ->
        assert (_menhir_env._menhir_shifted >= 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_error23 _menhir_env _menhir_stack

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Constraint.Raw.ty_constraint) = (
  CDump
) in
    _menhir_goto_tconstraint _menhir_env _menhir_stack _menhir_s _v

and input : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Constraint.Raw.ty_constraint) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_startp = lexbuf.Lexing.lex_start_p;
      _menhir_endp = lexbuf.Lexing.lex_curr_p;
      _menhir_shifted = 1073741823
    } in
    Obj.magic (let _menhir_stack = () in
    assert (_menhir_env._menhir_shifted <> (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DUMP ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EXISTS ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FALSE ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ID _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | INT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LET ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | STRING ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | TRUE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (_menhir_env._menhir_shifted >= 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_error0 _menhir_env _menhir_stack)



