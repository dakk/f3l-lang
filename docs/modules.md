# Module
In f3l every file is a module; but a syntax for nested modules is present:

```ocaml
module Another_module = struct
    def i3 = 3;
end
```

You can directly access a module in the current include_path, referencing by name:

```ocaml
Another_module_in_the_same_directory.hello ();
```


# Open

Otherwise, if you want to open a file as a module, you can use the open keyword instead.

```kotlin
open Amodule;
```

And then, you can use a definition from the opened module file without referencing the module name like this:

```
hello ();
```