{ lib }:

{ tree-sitter-blueprint
, ...
}:

{
  tree-sitter-blueprint = {
    src = lib.cleanSource tree-sitter-blueprint;
    generate = true;
  };
}
