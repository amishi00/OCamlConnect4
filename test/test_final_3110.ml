open Final_3110.State
open OUnit2

let tests =
  let rows = 6 in
  let cols = 7 in
  [
    ( "win" >:: fun _ ->
      let board = create_board () in
      assert (get_status board = InProgress);
      let board = make_move board Red 0 in
      let board = make_move board Blue 2 in
      let board = make_move board Red 0 in
      let board = make_move board Blue 2 in
      let board = make_move board Red 0 in
      let board = make_move board Blue 2 in
      let board = make_move board Red 0 in
      assert (get_status board = Won Red) );
    ( "testingEmptyBoard" >:: fun _ ->
      let board = create_board () in
      assert (Array.length board = rows);
      Array.iter
        (fun row ->
          assert (Array.length row = cols);
          Array.iter (fun cell -> assert (cell = None)) row)
        board );
    ( "draw" >:: fun _ ->
      let board =
        [|
          [|
            Some Red;
            Some Blue;
            Some Red;
            Some Blue;
            Some Red;
            Some Blue;
            Some Red;
          |];
          [|
            Some Red;
            Some Blue;
            Some Red;
            Some Blue;
            Some Red;
            Some Blue;
            Some Red;
          |];
          [|
            Some Blue;
            Some Red;
            Some Blue;
            Some Red;
            Some Blue;
            Some Red;
            Some Blue;
          |];
          [|
            Some Blue;
            Some Red;
            Some Blue;
            Some Red;
            Some Blue;
            Some Red;
            Some Blue;
          |];
          [|
            Some Red;
            Some Blue;
            Some Red;
            Some Blue;
            Some Red;
            Some Blue;
            Some Red;
          |];
          [|
            Some Red;
            Some Blue;
            Some Red;
            Some Blue;
            Some Red;
            Some Blue;
            Some Red;
          |];
        |]
      in
      assert (get_status board = Draw) );
    ( "string of board" >:: fun _ ->
      let board =
        [|
          [| Some Red; Some Blue; None; Some Blue; None; Some Red; None |];
          [| None; Some Red; Some Blue; None; Some Red; Some Blue; None |];
          [| Some Red; None; Some Blue; Some Red; Some Blue; None; Some Blue |];
          [| None; None; Some Red; Some Blue; Some Red; Some Blue; Some Red |];
          [| Some Red; Some Blue; None; None; Some Red; None; Some Blue |];
          [| None; Some Red; None; Some Blue; None; Some Red; Some Blue |];
        |]
      in
      let expected_output =
        ".R.B.RB\nRB..R.B\n..RBRBR\nR.BRB.B\n.RB.RB.\nRB.B.R."
      in
      assert (string_of_board board = expected_output) );
    ( "incorrect move attempt" >:: fun _ ->
      let board = create_board () in
      assert (get_status board = InProgress);
      try
        let _ = make_move board Red (-1) in
        assert_failure "Expected InvalidMove exception"
      with
      | InvalidMove _ -> ()
      | _ -> assert_failure "Unexpected exception occurred" );
  ]

let test_suite = " test suite" >::: tests
let _ = run_test_tt_main test_suite
