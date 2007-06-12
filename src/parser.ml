  

  open Positions
  open Syntax.Raw
  open Level

  let mkPos startpos endpos =
    Positions.lex_join startpos endpos

  let mkFun annot params t = 
    List.fold_right(fun b t -> ELambda (annot, (b, t))) params t

exception Error

type token = 
  | WITH
  | VALREC
  | VAL
  | TYSTRING
  | TYPE
  | TYINT
  | TID of (string)
  | STRING of (string)
  | STAR
  | RPAREN
  | PIPE
  | OF
  | MATCH
  | LPAREN
  | LID of (string)
  | LETREC
  | LET
  | LAMBDA
  | INT of (int)
  | IN
  | ID of (string)
  | EQ
  | EOF
  | END
  | DOT
  | COLON
  | BINOP of (string)
  | ARROW

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
  | MenhirState1
  | MenhirState6
  | MenhirState7
  | MenhirState8
  | MenhirState10
  | MenhirState11
  | MenhirState17
  | MenhirState20
  | MenhirState22
  | MenhirState26
  | MenhirState27
  | MenhirState30
  | MenhirState34
  | MenhirState39
  | MenhirState45
  | MenhirState49
  | MenhirState53
  | MenhirState61
  | MenhirState63
  | MenhirState64
  | MenhirState66
  | MenhirState68
  | MenhirState69
  | MenhirState71
  | MenhirState72
  | MenhirState74
  | MenhirState77
  | MenhirState78
  | MenhirState80
  | MenhirState81
  | MenhirState82
  | MenhirState84
  | MenhirState87
  | MenhirState88
  | MenhirState89
  | MenhirState90
  | MenhirState91
  | MenhirState92
  | MenhirState93
  | MenhirState96
  | MenhirState103
  | MenhirState104
  | MenhirState105
  | MenhirState109
  | MenhirState110

let _eRR =
  Error

let rec _menhir_goto_pattern : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.Raw.pattern) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (_menhir_env._menhir_shifted <> (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__3_ = _endpos in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, p, _startpos_p_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : (Syntax.Raw.pattern) = (
  p
) in
            _menhir_goto_pattern0 _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (_menhir_env._menhir_shifted <> (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ID _v ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INT _v ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LAMBDA ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp
            | LET ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp
            | LETREC ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp
            | LID _v ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LPAREN ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp
            | MATCH ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp
            | STRING _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | _ ->
                assert (_menhir_env._menhir_shifted >= 0);
                _menhir_env._menhir_shifted <- (-1);
                _menhir_error103 _menhir_env _menhir_stack)
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_nonempty_list_pattern0_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.Raw.pattern list) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _endpos ->
    match _menhir_s with
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let _endpos_xs_ = _endpos in
        let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
        let _endpos = _endpos_xs_ in
        let _v : (Syntax.Raw.pattern list) =     ( x :: xs ) in
        _menhir_goto_nonempty_list_pattern0_ _menhir_env _menhir_stack _menhir_s _v _endpos
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ps = _v in
        let _endpos_ps_ = _endpos in
        let (_menhir_stack, _menhir_s, k, _startpos_k_, _endpos_k_) = _menhir_stack in
        let _startpos = _startpos_k_ in
        let _endpos = _endpos_ps_ in
        let _v : (Syntax.Raw.pattern) = (
  let pos = mkPos _startpos _endpos in
    PDataCon (pos, k, ps)
) in
        _menhir_goto_pattern _menhir_env _menhir_stack _menhir_s _v _startpos
    | _ ->
        _menhir_fail ()

and _menhir_reduce31 : _menhir_env -> 'ttv_tail * _menhir_state * (string) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, k, _startpos_k_, _endpos_k_) = _menhir_stack in
    let _startpos = _startpos_k_ in
    let _endpos = _endpos_k_ in
    let _v : (Syntax.Raw.pattern) = (
  let pos = mkPos _startpos _endpos in
    PDataCon (pos, k, [])  
) in
    _menhir_goto_pattern0 _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run94 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack)

and _menhir_goto_pattern0 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.Raw.pattern) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    match _menhir_s with
    | MenhirState96 | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (_menhir_env._menhir_shifted <> (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LID _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAREN ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_startp
        | ARROW | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
            let _endpos = _endpos_x_ in
            let _v : (Syntax.Raw.pattern list) =     ( [ x ] ) in
            _menhir_goto_nonempty_list_pattern0_ _menhir_env _menhir_stack _menhir_s _v _endpos
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_error96 _menhir_env _menhir_stack)
    | MenhirState91 | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, p, _startpos_p_, _endpos_p_) = _menhir_stack in
        let _startpos = _startpos_p_ in
        let _v : (Syntax.Raw.pattern) = (
  p
) in
        _menhir_goto_pattern _menhir_env _menhir_stack _menhir_s _v _startpos
    | _ ->
        _menhir_fail ()

and _menhir_run92 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LID _v ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_startp
    | _ ->
        assert (_menhir_env._menhir_shifted >= 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_error92 _menhir_env _menhir_stack

and _menhir_run93 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LID _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_startp
    | ARROW | RPAREN ->
        _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (_menhir_env._menhir_shifted >= 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_error93 _menhir_env _menhir_stack

and _menhir_run95 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _startpos_x_ = _startpos in
    let _endpos_x_ = _endpos in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : (Syntax.Raw.pattern) = ( 
  let pos = mkPos _startpos _endpos in
    PVar (pos, x)
) in
    _menhir_goto_pattern0 _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_goto_separated_nonempty_list_STAR_ty_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.Raw.ty list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x, _endpos_x_) = _menhir_stack in
        let _v : (Syntax.Raw.ty list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_STAR_ty_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ty = _v in
        let (_menhir_stack, _menhir_s, k, _startpos_k_, _endpos_k_) = _menhir_stack in
        let _v : (Syntax.Raw.datatype_def) = (
  DataType (k, ty)
) in
        _menhir_goto_dt_definition _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_nonempty_list_preceded_PIPE_clause__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.Raw.clause list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _, x0), _, xs) = _menhir_stack in
        let _v : (Syntax.Raw.clause list) = let x =
          let x = x0 in
              ( x )
        in
            ( x :: xs ) in
        _menhir_goto_nonempty_list_preceded_PIPE_clause__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (_menhir_env._menhir_shifted <> (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__5_ = _endpos in
            let ((((_menhir_stack, _menhir_s, _startpos__1_), _, t, _startpos_t_, _endpos_t_), _), _, cs) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__5_ in
            let _v : (Syntax.Raw.term) = (
  let pos = mkPos _startpos _endpos in
    EMatch (pos, t, cs)
) in
            _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run91 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LID _v ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_startp
    | _ ->
        assert (_menhir_env._menhir_shifted >= 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_error91 _menhir_env _menhir_stack

and _menhir_run87 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.Raw.term) * Lexing.position * Lexing.position -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INT _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAMBDA ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_startp
    | LET ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_startp
    | LETREC ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_startp
    | LID _v ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_startp
    | MATCH ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_startp
    | STRING _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (_menhir_env._menhir_shifted >= 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_error87 _menhir_env _menhir_stack

and _menhir_goto_ty : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.Raw.ty) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _endpos) in
    match _menhir_s with
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, t1, _startpos_t1_, _endpos_t1_), _, t2, _endpos_t2_) = _menhir_stack in
        let _startpos = _startpos_t1_ in
        let _endpos = _endpos_t2_ in
        let _v : (Syntax.Raw.ty) = (
  let pos = mkPos _startpos _endpos in
    TyArrow (pos, t1, t2)
) in
        _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v _endpos
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (_menhir_env._menhir_shifted <> (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__3_ = _endpos in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, ty, _endpos_ty_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : (Syntax.Raw.ty) = (
  ty
) in
            _menhir_goto_ty0 _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState39 | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (_menhir_env._menhir_shifted <> (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ID _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LPAREN ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_startp
            | TID _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TYINT ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TYSTRING ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | _ ->
                assert (_menhir_env._menhir_shifted >= 0);
                _menhir_env._menhir_shifted <- (-1);
                _menhir_error39 _menhir_env _menhir_stack)
        | END | PIPE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x, _endpos_x_) = _menhir_stack in
            let _v : (Syntax.Raw.ty list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_STAR_ty_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (_menhir_env._menhir_shifted <> (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__5_ = _endpos in
            let (((_menhir_stack, _menhir_s, _startpos__1_), x, _startpos_x_, _endpos_x_), _, ty, _endpos_ty_) = _menhir_stack in
            let _v : (Syntax.Raw.binder) = (
  AnnotatedBind (x, ty)
) in
            _menhir_goto_binder _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (_menhir_env._menhir_shifted <> (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__5_ = _endpos in
            let ((((_menhir_stack, _menhir_s, _startpos__1_), _, t, _startpos_t_, _endpos_t_), _), _, ty, _endpos_ty_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__5_ in
            let _v : (Syntax.Raw.term) = (
  let pos = mkPos _startpos _endpos in
    EAnnot (pos, t, ty)
) in
            _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_nonempty_list_ty0_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.Raw.ty list) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _endpos ->
    match _menhir_s with
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let _endpos_xs_ = _endpos in
        let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
        let _endpos = _endpos_xs_ in
        let _v : (Syntax.Raw.ty list) =     ( x :: xs ) in
        _menhir_goto_nonempty_list_ty0_ _menhir_env _menhir_stack _menhir_s _v _endpos
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let args = _v in
        let _endpos_args_ = _endpos in
        let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_args_ in
        let _v : (Syntax.Raw.ty) = (
  let pos = mkPos _startpos _endpos in
    TyApp (pos, x, args)
) in
        _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v _endpos
    | _ ->
        _menhir_fail ()

and _menhir_goto_constant : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.Raw.primitive) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let c = _v in
    let _startpos_c_ = _startpos in
    let _endpos_c_ = _endpos in
    let _startpos = _startpos_c_ in
    let _endpos = _endpos_c_ in
    let _v : (Syntax.Raw.term) = (
  let pos = mkPos _startpos _endpos in
    EConstant (pos, c)
) in
    _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_goto_term : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.Raw.term) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    match _menhir_s with
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (_menhir_env._menhir_shifted <> (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INT _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LID _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAREN ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_startp
        | STRING _v ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | BINOP _ | COLON | END | EOF | IN | LET | LETREC | MATCH | PIPE | RPAREN | TYPE | VAL | VALREC | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, _startpos__1_), _, b), _, t, _startpos_t_, _endpos_t_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_t_ in
            let _v : (Syntax.Raw.term) = (
  let pos = mkPos _startpos _endpos in
    ELambda (pos, (b, t))
) in
            _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_error77 _menhir_env _menhir_stack)
    | MenhirState109 | MenhirState89 | MenhirState104 | MenhirState82 | MenhirState88 | MenhirState81 | MenhirState80 | MenhirState78 | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, t1, _startpos_t1_, _endpos_t1_), _, t2, _startpos_t2_, _endpos_t2_) = _menhir_stack in
        let _startpos = _startpos_t1_ in
        let _endpos = _endpos_t2_ in
        let _v : (Syntax.Raw.term) = (
  let pos = mkPos _startpos _endpos in
    EApp (pos, PrefixApp, t1, t2)
) in
        _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (_menhir_env._menhir_shifted <> (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INT _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LID _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAREN ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_startp
        | STRING _v ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | BINOP _ | COLON | END | EOF | IN | LET | LETREC | MATCH | PIPE | RPAREN | TYPE | VAL | VALREC | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, _startpos__1_), _, d), _, t, _startpos_t_, _endpos_t_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_t_ in
            let _v : (Syntax.Raw.term) = (
  let pos = mkPos _startpos _endpos in
  let (x, t') = d in
    ELet (pos, (x, t', t))
) in
            _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_error80 _menhir_env _menhir_stack)
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (_menhir_env._menhir_shifted <> (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INT _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LID _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAREN ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_startp
        | STRING _v ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | BINOP _ | COLON | END | EOF | IN | LET | LETREC | MATCH | PIPE | RPAREN | TYPE | VAL | VALREC | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, _startpos__1_), _, d), _, t, _startpos_t_, _endpos_t_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_t_ in
            let _v : (Syntax.Raw.term) = (
  let pos = mkPos _startpos _endpos in
  let (x, t') = d in
    ELetRec (pos, (x, t', t))
) in
            _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_error81 _menhir_env _menhir_stack)
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (_menhir_env._menhir_shifted <> (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BINOP _v ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState82 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ID _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LPAREN ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_startp
            | TID _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TYINT ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TYSTRING ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | _ ->
                assert (_menhir_env._menhir_shifted >= 0);
                _menhir_env._menhir_shifted <- (-1);
                _menhir_error84 _menhir_env _menhir_stack)
        | ID _v ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INT _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LAMBDA ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _menhir_env._menhir_startp
        | LET ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _menhir_env._menhir_startp
        | LETREC ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _menhir_env._menhir_startp
        | LID _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAREN ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _menhir_env._menhir_startp
        | MATCH ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _menhir_env._menhir_startp
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState82 in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__3_ = _endpos in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, t, _startpos_t_, _endpos_t_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : (Syntax.Raw.term) = (
  t
) in
            _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | STRING _v ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_error82 _menhir_env _menhir_stack)
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (_menhir_env._menhir_shifted <> (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INT _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LAMBDA ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_startp
        | LID _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAREN ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_startp
        | STRING _v ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | COLON | END | EOF | IN | PIPE | RPAREN | TYPE | VAL | VALREC | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, t1, _startpos_t1_, _endpos_t1_), _, c), _, t2, _startpos_t2_, _endpos_t2_) = _menhir_stack in
            let _startpos = _startpos_t1_ in
            let _endpos = _endpos_t2_ in
            let _v : (Syntax.Raw.term) = (
  let pos = mkPos _startpos _endpos in
  let op = c in
    EApp (pos, PrefixApp, EApp (pos, InfixApp 0, EVar (pos, op), t1), t2)
) in
            _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_error88 _menhir_env _menhir_stack)
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (_menhir_env._menhir_shifted <> (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BINOP _v ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
        | ID _v ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INT _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LAMBDA ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_startp
        | LET ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_startp
        | LETREC ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_startp
        | LID _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAREN ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_startp
        | MATCH ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_startp
        | STRING _v ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState89 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | PIPE ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | _ ->
                assert (_menhir_env._menhir_shifted >= 0);
                _menhir_env._menhir_shifted <- (-1);
                _menhir_error90 _menhir_env _menhir_stack)
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_error89 _menhir_env _menhir_stack)
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (_menhir_env._menhir_shifted <> (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BINOP _v ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
        | ID _v ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INT _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LAMBDA ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp
        | LET ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp
        | LETREC ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp
        | LID _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAREN ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp
        | MATCH ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp
        | STRING _v ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | END | PIPE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, p, _startpos_p_), _, t, _startpos_t_, _endpos_t_) = _menhir_stack in
            let _startpos = _startpos_p_ in
            let _endpos = _endpos_t_ in
            let _v : (Syntax.Raw.clause) = (
  let pos = mkPos _startpos _endpos in
    (pos, p, t)
) in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (_menhir_env._menhir_shifted <> (-1));
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | PIPE ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | END ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, x0) = _menhir_stack in
                let _v : (Syntax.Raw.clause list) = let x =
                  let x = x0 in
                      ( x )
                in
                    ( [ x ] ) in
                _menhir_goto_nonempty_list_preceded_PIPE_clause__ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (_menhir_env._menhir_shifted >= 0);
                _menhir_env._menhir_shifted <- (-1);
                _menhir_error105 _menhir_env _menhir_stack)
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_error104 _menhir_env _menhir_stack)
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (_menhir_env._menhir_shifted <> (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BINOP _v ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
        | ID _v ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INT _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LAMBDA ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_startp
        | LET ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_startp
        | LETREC ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_startp
        | LID _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAREN ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_startp
        | MATCH ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_startp
        | STRING _v ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | EOF | IN | TYPE | VAL | VALREC ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, x, _startpos_x_), _, params), _, t, _startpos_t_, _endpos_t_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_t_ in
            let _v : (Syntax.Raw.var * Syntax.Raw.term) = (
  let pos = mkPos _startpos _endpos in
    (x, mkFun pos params t)
) in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            (match _menhir_s with
            | MenhirState1 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (_menhir_env._menhir_shifted <> (-1));
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | EOF ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _menhir_env._menhir_endp
                | TYPE ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _menhir_env._menhir_startp
                | VAL ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _menhir_env._menhir_startp
                | VALREC ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _menhir_env._menhir_startp
                | _ ->
                    assert (_menhir_env._menhir_shifted >= 0);
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_error6 _menhir_env _menhir_stack)
            | MenhirState7 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (_menhir_env._menhir_shifted <> (-1));
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | EOF ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _menhir_env._menhir_endp
                | TYPE ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _menhir_env._menhir_startp
                | VAL ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _menhir_env._menhir_startp
                | VALREC ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _menhir_env._menhir_startp
                | _ ->
                    assert (_menhir_env._menhir_shifted >= 0);
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_error8 _menhir_env _menhir_stack)
            | MenhirState66 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (_menhir_env._menhir_shifted <> (-1));
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | IN ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _tok = _menhir_discard _menhir_env in
                    (match _tok with
                    | ID _v ->
                        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | INT _v ->
                        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | LAMBDA ->
                        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
                    | LET ->
                        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
                    | LETREC ->
                        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
                    | LID _v ->
                        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | LPAREN ->
                        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
                    | MATCH ->
                        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
                    | STRING _v ->
                        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | _ ->
                        assert (_menhir_env._menhir_shifted >= 0);
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_error68 _menhir_env _menhir_stack)
                | _ ->
                    assert (_menhir_env._menhir_shifted >= 0);
                    _menhir_env._menhir_shifted <- (-1);
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | MenhirState69 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (_menhir_env._menhir_shifted <> (-1));
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | IN ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _tok = _menhir_discard _menhir_env in
                    (match _tok with
                    | ID _v ->
                        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | INT _v ->
                        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | LAMBDA ->
                        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_startp
                    | LET ->
                        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_startp
                    | LETREC ->
                        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_startp
                    | LID _v ->
                        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | LPAREN ->
                        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_startp
                    | MATCH ->
                        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_startp
                    | STRING _v ->
                        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | _ ->
                        assert (_menhir_env._menhir_shifted >= 0);
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_error71 _menhir_env _menhir_stack)
                | _ ->
                    assert (_menhir_env._menhir_shifted >= 0);
                    _menhir_env._menhir_shifted <- (-1);
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                _menhir_fail ())
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_error109 _menhir_env _menhir_stack)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_PIPE_dt_definition_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.Raw.datatype_def list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (Syntax.Raw.datatype_def list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_PIPE_dt_definition__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (Syntax.Raw.datatype_def list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_PIPE_dt_definition_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_tyconstant : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.Raw.ty_constant) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let c = _v in
    let _startpos_c_ = _startpos in
    let _endpos_c_ = _endpos in
    let _startpos = _startpos_c_ in
    let _endpos = _endpos_c_ in
    let _v : (Syntax.Raw.ty) = (
  let pos = mkPos _startpos _endpos in
    TyConstant (pos, c)
) in
    _menhir_goto_ty0 _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_goto_ty0 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.Raw.ty) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    match _menhir_s with
    | MenhirState30 | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (_menhir_env._menhir_shifted <> (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAREN ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_startp
        | TID _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | TYINT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | TYSTRING ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | END | PIPE | RPAREN | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
            let _endpos = _endpos_x_ in
            let _v : (Syntax.Raw.ty list) =     ( [ x ] ) in
            _menhir_goto_nonempty_list_ty0_ _menhir_env _menhir_stack _menhir_s _v _endpos
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_error30 _menhir_env _menhir_stack)
    | MenhirState84 | MenhirState53 | MenhirState39 | MenhirState22 | MenhirState34 | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (_menhir_env._menhir_shifted <> (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ID _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LPAREN ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _menhir_env._menhir_startp
            | TID _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TYINT ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TYSTRING ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | _ ->
                assert (_menhir_env._menhir_shifted >= 0);
                _menhir_env._menhir_shifted <- (-1);
                _menhir_error34 _menhir_env _menhir_stack)
        | END | PIPE | RPAREN | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, ty, _startpos_ty_, _endpos_ty_) = _menhir_stack in
            let _endpos = _endpos_ty_ in
            let _v : (Syntax.Raw.ty) = (
  ty
) in
            _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v _endpos
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce59 : _menhir_env -> 'ttv_tail * _menhir_state * (string) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : (Syntax.Raw.ty) = (
  let pos = mkPos _startpos _endpos in
    TyApp (pos, x, [])
) in
    _menhir_goto_ty0 _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run28 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce59 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run62 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _startpos_x_ = _startpos in
    let _endpos_x_ = _endpos in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : (Syntax.Raw.primitive) = (
  String x
) in
    _menhir_goto_constant _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run63 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INT _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAMBDA ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp
    | LET ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp
    | LETREC ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp
    | LID _v ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp
    | MATCH ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp
    | STRING _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (_menhir_env._menhir_shifted >= 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_error63 _menhir_env _menhir_stack

and _menhir_run64 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INT _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAMBDA ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_startp
    | LET ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_startp
    | LETREC ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_startp
    | LID _v ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_startp
    | MATCH ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_startp
    | STRING _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (_menhir_env._menhir_shifted >= 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_error64 _menhir_env _menhir_stack

and _menhir_run65 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _startpos_x_ = _startpos in
    let _endpos_x_ = _endpos in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : (Syntax.Raw.term) = (
  let pos = mkPos _startpos _endpos in
    EVar (pos, x)
) in
    _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run66 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_startp
    | _ ->
        assert (_menhir_env._menhir_shifted >= 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_error66 _menhir_env _menhir_stack

and _menhir_run69 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_startp
    | _ ->
        assert (_menhir_env._menhir_shifted >= 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_error69 _menhir_env _menhir_stack

and _menhir_run72 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_startp
    | _ ->
        assert (_menhir_env._menhir_shifted >= 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_error72 _menhir_env _menhir_stack

and _menhir_run75 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _startpos_x_ = _startpos in
    let _endpos_x_ = _endpos in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : (Syntax.Raw.primitive) = (
  Int x
) in
    _menhir_goto_constant _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run76 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _startpos_x_ = _startpos in
    let _endpos_x_ = _endpos in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : (Syntax.Raw.term) = (
  let pos = mkPos _startpos _endpos in
    EVar (pos, x)
) in
    _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_goto_dt_definition : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.Raw.datatype_def) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (_menhir_env._menhir_shifted <> (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | PIPE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | LID _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_error45 _menhir_env _menhir_stack)
    | END ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (Syntax.Raw.datatype_def list) =     ( [ x ] ) in
        _menhir_goto_separated_nonempty_list_PIPE_dt_definition_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (_menhir_env._menhir_shifted >= 0);
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_list_binder_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.Raw.binder list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (_menhir_env._menhir_shifted <> (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ID _v ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INT _v ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LAMBDA ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp
            | LET ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp
            | LETREC ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp
            | LID _v ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LPAREN ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp
            | MATCH ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp
            | STRING _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | _ ->
                assert (_menhir_env._menhir_shifted >= 0);
                _menhir_env._menhir_shifted <- (-1);
                _menhir_error61 _menhir_env _menhir_stack)
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (Syntax.Raw.binder list) =     ( x :: xs ) in
        _menhir_goto_list_binder_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _endpos__1_ = _endpos in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (Syntax.Raw.ty_constant) = (
  TyString
) in
    _menhir_goto_tyconstant _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _endpos__1_ = _endpos in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (Syntax.Raw.ty_constant) = (
  TyInt
) in
    _menhir_goto_tyconstant _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _startpos_x_ = _startpos in
    let _endpos_x_ = _endpos in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : (Syntax.Raw.ty) = (
  let pos = mkPos _startpos _endpos in
    TyVar (pos, x)
) in
    _menhir_goto_ty0 _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_startp
    | TID _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TYINT ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TYSTRING ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (_menhir_env._menhir_shifted >= 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_error26 _menhir_env _menhir_stack

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_startp
    | TID _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TYINT ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TYSTRING ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | ARROW | END | PIPE | RPAREN | STAR ->
        _menhir_reduce59 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (_menhir_env._menhir_shifted >= 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_error27 _menhir_env _menhir_stack

and _menhir_goto_binop : _menhir_env -> 'ttv_tail -> (Syntax.Raw.var) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (_menhir_env._menhir_shifted <> (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_endp in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos__3_ = _endpos in
        let ((_menhir_stack, _menhir_s, _startpos__1_), b) = _menhir_stack in
        let _v : (Syntax.Raw.binder) = (
  Bind b
) in
        _menhir_goto_binder _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (_menhir_env._menhir_shifted >= 0);
        _menhir_env._menhir_shifted <- (-1);
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_binder : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.Raw.binder) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (_menhir_env._menhir_shifted <> (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ID _v ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INT _v ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LAMBDA ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_startp
            | LET ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_startp
            | LETREC ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_startp
            | LID _v ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LPAREN ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_startp
            | MATCH ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_startp
            | STRING _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | _ ->
                assert (_menhir_env._menhir_shifted >= 0);
                _menhir_env._menhir_shifted <- (-1);
                _menhir_error74 _menhir_env _menhir_stack)
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState110 | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (_menhir_env._menhir_shifted <> (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAREN ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp
        | EQ ->
            _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_error110 _menhir_env _menhir_stack)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_PIPE_dt_definition__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.Raw.datatype_def list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let xs0 = _v in
    let (_menhir_stack, _) = _menhir_stack in
    let _v : (Syntax.Raw.datatype_def list) = let dts =
      let xs = xs0 in
          ( xs )
    in
    (
  dts
) in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let i = _v in
    let _v : (Syntax.Raw.type_definition) = (
  AlgebraicDataType i
) in
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (_menhir_env._menhir_shifted <> (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | END ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_endp in
        let _menhir_stack = (_menhir_stack, _endpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | EOF ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_endp
        | TYPE ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_startp
        | VAL ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_startp
        | VALREC ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_startp
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_error17 _menhir_env _menhir_stack)
    | _ ->
        assert (_menhir_env._menhir_shifted >= 0);
        _menhir_env._menhir_shifted <- (-1);
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | OF ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | ID _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAREN ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _menhir_env._menhir_startp
        | TID _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | TYINT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | TYSTRING ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_error22 _menhir_env _menhir_stack)
    | END | PIPE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, k, _startpos_k_, _endpos_k_) = _menhir_stack in
        let _v : (Syntax.Raw.datatype_def) = (
  DataType (k, [])
) in
        _menhir_goto_dt_definition _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (_menhir_env._menhir_shifted >= 0);
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce16 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Syntax.Raw.binder list) =     ( [] ) in
    _menhir_goto_list_binder_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run50 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BINOP _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let b = _v in
        let _v : (Syntax.Raw.var) = ( 
  b
) in
        _menhir_goto_binop _menhir_env _menhir_stack _v
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _endpos = _menhir_env._menhir_endp in
        let _menhir_stack = (_menhir_stack, _v, _startpos, _endpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ID _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LPAREN ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_startp
            | TID _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TYINT ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TYSTRING ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | _ ->
                assert (_menhir_env._menhir_shifted >= 0);
                _menhir_env._menhir_shifted <- (-1);
                _menhir_error53 _menhir_env _menhir_stack)
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | STAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _v : (Syntax.Raw.var) = (
  "*"
) in
        _menhir_goto_binop _menhir_env _menhir_stack _v
    | _ ->
        assert (_menhir_env._menhir_shifted >= 0);
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run59 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _startpos_x_ = _startpos in
    let _endpos_x_ = _endpos in
    let _v : (Syntax.Raw.binder) = (
  Bind x
) in
    _menhir_goto_binder _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_option_PIPE_ : _menhir_env -> 'ttv_tail -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (_menhir_env._menhir_shifted <> (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LID _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | END ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState20 in
        let _v : (Syntax.Raw.datatype_def list) =     ( [] ) in
        _menhir_goto_loption_separated_nonempty_list_PIPE_dt_definition__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (_menhir_env._menhir_shifted >= 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_error20 _menhir_env _menhir_stack

and _menhir_goto_identifier : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.Raw.var) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (_menhir_env._menhir_shifted <> (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp
    | EQ ->
        _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | _ ->
        assert (_menhir_env._menhir_shifted >= 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_error49 _menhir_env _menhir_stack

and _menhir_error110 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.Raw.binder) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error109 : _menhir_env -> (('ttv_tail * _menhir_state * (Syntax.Raw.var) * Lexing.position) * _menhir_state * (Syntax.Raw.binder list)) * _menhir_state * (Syntax.Raw.term) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error105 : _menhir_env -> (((('ttv_tail * _menhir_state * Lexing.position) * _menhir_state * (Syntax.Raw.term) * Lexing.position * Lexing.position) * _menhir_state) * _menhir_state) * _menhir_state * (Syntax.Raw.clause) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error104 : _menhir_env -> ((((('ttv_tail * _menhir_state * Lexing.position) * _menhir_state * (Syntax.Raw.term) * Lexing.position * Lexing.position) * _menhir_state) * _menhir_state) * _menhir_state * (Syntax.Raw.pattern) * Lexing.position) * _menhir_state * (Syntax.Raw.term) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error103 : _menhir_env -> (((('ttv_tail * _menhir_state * Lexing.position) * _menhir_state * (Syntax.Raw.term) * Lexing.position * Lexing.position) * _menhir_state) * _menhir_state) * _menhir_state * (Syntax.Raw.pattern) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error96 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.Raw.pattern) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error93 : _menhir_env -> 'ttv_tail * _menhir_state * (string) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error92 : _menhir_env -> 'ttv_tail * _menhir_state * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error91 : _menhir_env -> 'ttv_tail * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error90 : _menhir_env -> (('ttv_tail * _menhir_state * Lexing.position) * _menhir_state * (Syntax.Raw.term) * Lexing.position * Lexing.position) * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error89 : _menhir_env -> ('ttv_tail * _menhir_state * Lexing.position) * _menhir_state * (Syntax.Raw.term) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error88 : _menhir_env -> (('ttv_tail * _menhir_state * (Syntax.Raw.term) * Lexing.position * Lexing.position) * _menhir_state * (string)) * _menhir_state * (Syntax.Raw.term) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error87 : _menhir_env -> ('ttv_tail * _menhir_state * (Syntax.Raw.term) * Lexing.position * Lexing.position) * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error84 : _menhir_env -> (('ttv_tail * _menhir_state * Lexing.position) * _menhir_state * (Syntax.Raw.term) * Lexing.position * Lexing.position) * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error82 : _menhir_env -> ('ttv_tail * _menhir_state * Lexing.position) * _menhir_state * (Syntax.Raw.term) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error81 : _menhir_env -> (('ttv_tail * _menhir_state * Lexing.position) * _menhir_state * (Syntax.Raw.var * Syntax.Raw.term)) * _menhir_state * (Syntax.Raw.term) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error80 : _menhir_env -> (('ttv_tail * _menhir_state * Lexing.position) * _menhir_state * (Syntax.Raw.var * Syntax.Raw.term)) * _menhir_state * (Syntax.Raw.term) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error77 : _menhir_env -> (('ttv_tail * _menhir_state * Lexing.position) * _menhir_state * (Syntax.Raw.binder)) * _menhir_state * (Syntax.Raw.term) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error74 : _menhir_env -> ('ttv_tail * _menhir_state * Lexing.position) * _menhir_state * (Syntax.Raw.binder) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error72 : _menhir_env -> 'ttv_tail * _menhir_state * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error71 : _menhir_env -> ('ttv_tail * _menhir_state * Lexing.position) * _menhir_state * (Syntax.Raw.var * Syntax.Raw.term) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error69 : _menhir_env -> 'ttv_tail * _menhir_state * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error68 : _menhir_env -> ('ttv_tail * _menhir_state * Lexing.position) * _menhir_state * (Syntax.Raw.var * Syntax.Raw.term) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error66 : _menhir_env -> 'ttv_tail * _menhir_state * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error64 : _menhir_env -> 'ttv_tail * _menhir_state * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error63 : _menhir_env -> 'ttv_tail * _menhir_state * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error61 : _menhir_env -> ('ttv_tail * _menhir_state * (Syntax.Raw.var) * Lexing.position) * _menhir_state * (Syntax.Raw.binder list) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error53 : _menhir_env -> (('ttv_tail * _menhir_state * (Syntax.Raw.var) * Lexing.position) * _menhir_state * Lexing.position) * (string) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error49 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.Raw.var) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error45 : _menhir_env -> (((('ttv_tail * _menhir_state * Lexing.position) * (string) * Lexing.position * Lexing.position) * _menhir_state * (Syntax.Raw.param list)) * (unit option)) * _menhir_state * (Syntax.Raw.datatype_def) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error39 : _menhir_env -> ((((('ttv_tail * _menhir_state * Lexing.position) * (string) * Lexing.position * Lexing.position) * _menhir_state * (Syntax.Raw.param list)) * (unit option)) * _menhir_state * (string) * Lexing.position * Lexing.position) * _menhir_state * (Syntax.Raw.ty) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error34 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.Raw.ty) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error30 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.Raw.ty) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error27 : _menhir_env -> 'ttv_tail * _menhir_state * (string) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error26 : _menhir_env -> 'ttv_tail * _menhir_state * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error22 : _menhir_env -> (((('ttv_tail * _menhir_state * Lexing.position) * (string) * Lexing.position * Lexing.position) * _menhir_state * (Syntax.Raw.param list)) * (unit option)) * _menhir_state * (string) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error20 : _menhir_env -> ((('ttv_tail * _menhir_state * Lexing.position) * (string) * Lexing.position * Lexing.position) * _menhir_state * (Syntax.Raw.param list)) * (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error17 : _menhir_env -> (((('ttv_tail * _menhir_state * Lexing.position) * (string) * Lexing.position * Lexing.position) * _menhir_state * (Syntax.Raw.param list)) * (Syntax.Raw.type_definition)) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error8 : _menhir_env -> ('ttv_tail * _menhir_state * Lexing.position) * _menhir_state * (Syntax.Raw.var * Syntax.Raw.term) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error6 : _menhir_env -> ('ttv_tail * _menhir_state * Lexing.position) * _menhir_state * (Syntax.Raw.var * Syntax.Raw.term) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_list_TID_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.Raw.param list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _, xs) = _menhir_stack in
        let _v : (Syntax.Raw.param list) =     ( x :: xs ) in
        _menhir_goto_list_TID_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (_menhir_env._menhir_shifted <> (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | PIPE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _ = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let x = () in
                let _v : (unit option) =     ( Some x ) in
                _menhir_goto_option_PIPE_ _menhir_env _menhir_stack _v
            | END | LID _ ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _v : (unit option) =     ( None ) in
                _menhir_goto_option_PIPE_ _menhir_env _menhir_stack _v
            | _ ->
                assert (_menhir_env._menhir_shifted >= 0);
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_error11 : _menhir_env -> 'ttv_tail * _menhir_state * (string) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_error1 : _menhir_env -> 'ttv_tail * _menhir_state * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error7 : _menhir_env -> 'ttv_tail * _menhir_state * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BINOP _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__3_ = _endpos in
            let ((_menhir_stack, _menhir_s, _startpos__1_), x) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _v : (Syntax.Raw.var) = ( 
  x 
) in
            _menhir_goto_identifier _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (_menhir_env._menhir_shifted >= 0);
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _startpos_x_ = _startpos in
    let _endpos_x_ = _endpos in
    let _startpos = _startpos_x_ in
    let _v : (Syntax.Raw.var) = ( 
  x 
) in
    _menhir_goto_identifier _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState0 ->
        _menhir_error0 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState1 ->
        _menhir_error1 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState6 ->
        _menhir_error6 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState7 ->
        _menhir_error7 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState8 ->
        _menhir_error8 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState10 ->
        _menhir_error10 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState11 ->
        _menhir_error11 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState17 ->
        _menhir_error17 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState20 ->
        _menhir_error20 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState22 ->
        _menhir_error22 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState26 ->
        _menhir_error26 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState27 ->
        _menhir_error27 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState30 ->
        _menhir_error30 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState34 ->
        _menhir_error34 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState39 ->
        _menhir_error39 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState45 ->
        _menhir_error45 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState49 ->
        _menhir_error49 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState53 ->
        _menhir_error53 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState61 ->
        _menhir_error61 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState63 ->
        _menhir_error63 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState64 ->
        _menhir_error64 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState66 ->
        _menhir_error66 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState68 ->
        _menhir_error68 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState69 ->
        _menhir_error69 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState71 ->
        _menhir_error71 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState72 ->
        _menhir_error72 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState74 ->
        _menhir_error74 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState77 ->
        _menhir_error77 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        _menhir_error80 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState81 ->
        _menhir_error81 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState82 ->
        _menhir_error82 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState84 ->
        _menhir_error84 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState87 ->
        _menhir_error87 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState88 ->
        _menhir_error88 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState89 ->
        _menhir_error89 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState90 ->
        _menhir_error90 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState91 ->
        _menhir_error91 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState92 ->
        _menhir_error92 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState93 ->
        _menhir_error93 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState96 ->
        _menhir_error96 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState103 ->
        _menhir_error103 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState104 ->
        _menhir_error104 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState105 ->
        _menhir_error105 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState109 ->
        _menhir_error109 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState110 ->
        _menhir_error110 _menhir_env (Obj.magic _menhir_stack)

and _menhir_error10 : _menhir_env -> ('ttv_tail * _menhir_state * Lexing.position) * (string) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Syntax.Raw.param list) =     ( [] ) in
    _menhir_goto_list_TID_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | TID _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | EQ ->
        _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | _ ->
        assert (_menhir_env._menhir_shifted >= 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_error11 _menhir_env _menhir_stack

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

and _menhir_goto_program : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.Raw.program) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _endpos ->
    match _menhir_s with
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let p = _v in
        let _endpos_p_ = _endpos in
        let (((((_menhir_stack, _menhir_s, _startpos__1_), ty00, _startpos_ty00_, _endpos_ty00_), _, params00), d00), _endpos__6_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_p_ in
        let _v : (Syntax.Raw.program) = let td =
          let d0 = d00 in
          let params0 = params00 in
          let ty0 = ty00 in
          let d =
            let d = d0 in
            let params = params0 in
            let ty = ty0 in
            (
  TypeDefinition (ty, params, d)
)
          in
          (
  d
)
        in
        (
  let pos = mkPos _startpos _endpos in
    NewDefinition (pos, (td, p))
) in
        _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v _endpos
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let p = _v in
        let _endpos_p_ = _endpos in
        let ((_menhir_stack, _menhir_s, _startpos__1_), _, d00) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_p_ in
        let _v : (Syntax.Raw.program) = let td =
          let d0 = d00 in
          let d =
            let d = d0 in
            (
  let (x, d) = d in
    ValDefinition (x, d)
)
          in
          (
  d
)
        in
        (
  let pos = mkPos _startpos _endpos in
    NewDefinition (pos, (td, p))
) in
        _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v _endpos
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let p = _v in
        let _endpos_p_ = _endpos in
        let ((_menhir_stack, _menhir_s, _startpos__1_), _, d00) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_p_ in
        let _v : (Syntax.Raw.program) = let td =
          let d0 = d00 in
          let d =
            let d = d0 in
            (
  let (x, d) = d in
    RecDefinitions (x, d)
)
          in
          (
  d
)
        in
        (
  let pos = mkPos _startpos _endpos in
    NewDefinition (pos, (td, p))
) in
        _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v _endpos
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = _v in
        let _endpos__1_ = _endpos in
        Obj.magic _1
    | _ ->
        _menhir_fail ()

and _menhir_error0 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _menhir_env._menhir_startp
    | _ ->
        assert (_menhir_env._menhir_shifted >= 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_error1 _menhir_env _menhir_stack

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _menhir_env._menhir_startp
    | _ ->
        assert (_menhir_env._menhir_shifted >= 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_error7 _menhir_env _menhir_stack

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _endpos = _menhir_env._menhir_endp in
        let _menhir_stack = (_menhir_stack, _v, _startpos, _endpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | TID _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | EQ ->
            _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | _ ->
            assert (_menhir_env._menhir_shifted >= 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_error10 _menhir_env _menhir_stack)
    | _ ->
        assert (_menhir_env._menhir_shifted >= 0);
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _endpos ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos__1_ = _endpos in
    let _endpos = _endpos__1_ in
    let _v : (Syntax.Raw.program) = (
  EmptyProgram
) in
    _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v _endpos

and program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Syntax.Raw.program) =
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
    | EOF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_endp
    | TYPE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_startp
    | VAL ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_startp
    | VALREC ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_startp
    | _ ->
        assert (_menhir_env._menhir_shifted >= 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_error0 _menhir_env _menhir_stack)



