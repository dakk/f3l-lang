# Open

If you want to open a file as a module, you can use the open keyword instead.

```ocaml
open Amodule;
```

And then, you can use a definition from the opened module file without referencing the module name like this:

```
hello ();
```