# f3l

F3L is a low level function language inspired by Ocaml and f3l-lang.

This is only a research project, it is not \(yet\) intendeed for real usage.


## Usage

```text
f3l-lang compiler

  f3l.exe ACTION FILENAME

=== actions ===

  compile file.f3l [-target c]
                 compiles file.f3l to target language c



=== flags ===

  [-no-remove-unused]  disable removing unused symbols
  [-print-ast]         print ast
  [-print-pt]          print parse-tree
  [-target _]          target language
  [-verbose]           enable verbosity
  [-build-info]        print info about this build and exit
  [-version]           print the version of this build and exit
  [-help]              print this help text and exit
                       (alias: -?)
```


## License

```text
Copyright (c) 2022 Davide Gessa

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
```

