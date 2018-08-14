let
  define = name: val: { ${name} = val; };
  key = "hello";
in
  define "key" "value" // {
    "${key}".world = ":D";
  }
