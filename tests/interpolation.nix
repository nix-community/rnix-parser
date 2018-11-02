let
  world = "World";
in {
  string = "Hello ${world}!";
  multiline = ''''\n
    The set's x value is: ${
      {
        x = "1";
        y = "2";
      }.x
    }

    This line shall have no indention
      This line shall be indented by 2
    ''\n
    ${ "hi" }
  '';
}
