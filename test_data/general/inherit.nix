let
  y = 2;
  set = { z = 3; a = 4; b = 5; };
in {
  x = 1;
  inherit y;
  inherit (set) z a;
}
