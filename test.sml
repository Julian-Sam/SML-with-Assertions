functor MkTreapTable(structure HashKey : HASHKEY) : TABLE =
  MkBSTTable(structure Tree = MkTreap)
