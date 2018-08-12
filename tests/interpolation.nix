let
  world = "World";
in {
  string = "Hello ${world}!";
  multiline = ''
    The set's x value is: ${
      {
        x = "1";
        y = "2";
      }.x
    }
  '';
}
