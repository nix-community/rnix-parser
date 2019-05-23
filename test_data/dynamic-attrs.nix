let
  define = name: val: { ${name} = val; };
  key = "hello";
  set = { a = 1; b = 2; };
  dynamic_key = "c";
in
  define "key" "value" // {
    "${key}".world = ":D";
    dynamic_key_set = set ? ${dynamic_key};
  }
