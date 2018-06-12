Control.Print.printDepth  := 50;
Control.Print.printLength := 50;

(* Tagged trees are binary trees whose nodes are pairs
   consisting of a tag and some data. Tags are not necessarily 
   unique in a tree, nor are tagged trees necessarily ordered.
 *)
type tag = int
type 'a entry = tag * 'a
datatype 'a tree = Empty
                 | Node of 'a tree * 'a entry * 'a tree
