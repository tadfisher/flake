{ lib, tree-sitter }:

{ tree-sitter-blueprint
, tree-sitter-typespec
, ...
}:

{
  tree-sitter-blueprint = tree-sitter.buildGrammar {
    language = "blueprint";
    version = "1.0.0";
    src = lib.cleanSource tree-sitter-blueprint;
    generate = true;
  };

  tree-sitter-typespec = tree-sitter.buildGrammar {
    language = "typespec";
    src = lib.cleanSource tree-sitter-typespec;
    version = "0.0.1";
    generate = true;
  };
}
