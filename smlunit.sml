structure SMLUnit = struct
  exception TestFailed;

  fun ok () = print ("OK\n");
  fun fail () = raise TestFailed;

  fun assert func args =
    if (func args) then ok () else fail ()

  fun assertNot func args =
    if not (func args) then ok () else fail ()

  fun assertThrow func args expectedException =
    (func args; fail ()) handle expectedException => ok ()

  fun assertEqual func args expected = let
      val actual = func args
  in
      if actual = expected then ok () else fail ()
  end
end
