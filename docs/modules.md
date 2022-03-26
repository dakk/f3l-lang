# Module
In f3l every file is a module; but a syntax for nested modules is present:

```ocaml
module Another_module = struct
    def i3 = 3;
end
```

# Include

Inside a f3l file, you can include another f3l module containing useful declarations. This will copy all the declarations to the current file.

```kotlin
include Anotherfile;
```


# Open

Otherwise, if you want to open a file as a module, you can use the open keyword instead.

```kotlin
open Amodule;
```

And then, you can use a definition from the opened file like this:

```
Amodule.hello ();
```