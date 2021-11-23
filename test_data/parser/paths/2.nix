let
  a = f: ./foo${bar}/baz;
  b = a ./bar ./baz${x} ./snens${x}y;
in b
