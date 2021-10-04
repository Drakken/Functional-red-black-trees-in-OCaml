

This is a module for using red-black trees in OCaml. It includes a simple tree
with elements that can be compared with (<), (=), and (>), along with a functor
for more complicated applications. Against Chris Okasaki's advice*, it uses
a functional version of the conventional rebalancing system, which searches
for red nodes near a node that has just been deleted. The rebalancing function 
is called at each step as the recursion of the delete function unwinds, even 
after the tree has been rebalanced, but the function's return value carries
a flag that indicates whether the rebalancing is done, so all it has to do is 
inspect the flag and return.


 
*Chris Okasaki, Red-Black Trees in a Functional Setting, J. Functional Programming, January 1993

