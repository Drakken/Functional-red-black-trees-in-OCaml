(*
 * a functional implementation of red-black trees
 * copyright (c) 2021 Daniel S. Bensen
 *)

type color = Red | Black

type 'a t = Empty | Node of 'a t * 'a * 'a t * color

let empty = Empty

let rec is_member x = function
  | Empty -> false
  | Node (l,y,r,_) -> if      x < y then is_member x l
                      else if x > y then is_member x r
                      else true 

let rec size = function
  | Empty -> 0
  | Node (l,_,r,_) -> size l + 1 + size r

let black_height tree =
  let rec bh = function
    | Empty -> 1
    | Node (l,_,_,c) -> bh l + (if c=Black then 1 else 0)
  in match tree with
    | Empty -> 0
    | Node (l,_,_,_) -> bh l

let rec map f = function
  | Empty -> Empty
  | Node(l,x,r,c) -> Node (map f l, f x, map f r, c)

let rec fold_left f acc = function
  | Empty -> acc
  | Node (l,x,r,_) -> fold_left f (f (fold_left f acc l) x) r

let rec fold_right f acc = function
  | Empty -> acc
  | Node (l,x,r,_) -> fold_right f (f (fold_right f acc r) x) l

let rec iter f = function
  | Empty -> ()
  | Node(l,x,r,_) -> iter f l; f x; iter f r

let iteri f root =
  let rec itr i0 = function
    | Empty -> i0
    | Node (l,x,r,_) ->
       let i1 = itr i0 l in
       (f i1 x; itr (i1+1) r) 
  in (let _ = itr 0 root in ())

let mapi f root =
  let rec map i0 = function
    | Empty -> (i0,Empty)
    | Node (lx,x,rx,c) ->
       let (i1,lf) = map i0 lx in
       let (i2,rf) = map (i1+1) rx in
       (i2, Node (lf, f i1 x, rf, c)) 
  in snd (map 0 root)


(***************************   insert   *******************************)

exception Already_in_tree

let is_red = function Node (_,_,_,Red) -> true | _ -> false

let is_black node = not (is_red node)

let   bad_node () = invalid_arg "unexpected node"
let empty_node () = invalid_arg   "empty node"

let right_child = function Node (_,_,r,_) -> r | Empty -> empty_node()
let  left_child = function Node (l,_,_,_) -> l | Empty -> empty_node()

let blacken = function
  | Node (l,x,r,Red) -> Node (l,x,r,Black)
  | tree -> tree

let node (l,x,r,c) = Node (l,x,r,c)


module Insert = struct            (* functions that use comparison operators *)

  let insert (<<) tree x dups_are_ok =                    (* rotations and recolorings are *)
    let rec ins xmino n = node (tuple xmino n)            (* folded together in each case  *)
    and tuple xmino = function                            (*   see ascii art for details   *)
    | Empty -> let tx = (Empty,x,Empty,Red) in
               ( match xmino with None -> tx
                                | Some xmin -> if xmin << x || dups_are_ok then tx
                                               else raise Already_in_tree )
    | Node (lz,z,rz,Black) -> 
      if x << z                                                       (* left side of black node *)
      then match lz with 
         | Node (ly,y,ry,Red) ->                                                    (* red child *)
           if x << y
           then let lynew = ins xmino ly in                                  (* outer grandchild *)
                if is_black lynew then (Node(lynew,y,ry,Red),z,rz,Black)
                else  (blacken lynew, y, Node(ry,z,rz,Black),Red)
           else                                                              (* inner grandchild *)
             begin
                match ins (Some y) ry with
                |  Node (lx,xnew,rx,Red) -> (Node(ly,y, lx,  Black),xnew,Node(rx,z,rz,Black), Red ) 
                |         rynew          -> (Node(ly,y,rynew, Red ), z,            rz,       Black)
             end
         | _ -> (ins xmino lz, z, rz, Black)
      else                                                         (* right side of black node:  *)
      begin                                                        (*  mirror image of left side *)
        match rz with
         | Node (ly,y,ry,Red) -> 
           if x << y
           then match ins (Some z) ly with
                |  Node (lx,xnew,rx,Red) -> (Node(lz,z,lx,Black),xnew,Node( rx,  y,ry,Black), Red )
                |         lynew          -> (         lz,         z,  Node(lynew,y,ry, Red ),Black)
           else let rynew = ins (Some y) ry in
                if is_black rynew then (lz,z,Node(ly,y,rynew,Red),Black)
                else (Node(lz,z,ly,Black), y, blacken rynew, Red)
         | _ -> (lz, z, ins (Some z) rz, Black)
      end
    | _ -> invalid_arg "red child of red node"
    in
    blacken (ins None (blacken tree))

  let insert_new (<<) tree x =
    try insert (<<) tree x false
    with Already_in_tree -> tree

  let merge insert t1 t2 = fold_left insert t2 t1

  let union insert t1 t2 =
    if black_height t1 < black_height t2
    then merge insert t1 t2
    else merge insert t2 t1

end

let insert     tree x = Insert.insert     (<) tree x true
let insert_new tree x = Insert.insert_new (<) tree x

let merge t1 t2 = Insert.union insert     t1 t2
let union t1 t2 = Insert.union insert_new t1 t2

let of_list xs = List.fold_left insert Empty xs

let to_list tree = fold_right (fun xs x -> x::xs) [] tree


(***************************   delete   *******************************)

(* KEY: c = color, l = left, n = node, p = parent, r = right, t = tuple, x = element *)

let first (l,_,_,_) = l
let third (_,_,r,_) = r

let lxrc = function
  | Node (l,x,r,c) -> (l,x,r,c)
  | Empty -> empty_node()

let redden = function
  | Node (l,x,r,Black) -> Node (l,x,r,Red)
  | _ -> invalid_arg "red or empty"

(* rebalancing *)

module Left = struct

  let case1 = function
    | (nx, p, Node (lw,w,rw,Red), Black) -> (Node(nx,p,lw,Red),w,rw)
    | _ -> bad_node()

  let case2 (nx,p,nw,cp) = (cp , Node (nx,p,redden nw,Black))

  let case34 = function
    | (nx, p, Node (Node(llw,xlw,rlw,Red),w,rw,Black), cp)
    -> (Red , Node (Node(nx,p,llw,Black), xlw,             (* case4 (case3 tp) *)
                    Node(rlw,w,rw,Black), cp))             (*  see ascii art   *)
    | _ -> bad_node()

  let case4 = function
    | (nx, p, Node(lw,w,rw,Black), cp) -> (Red , Node (Node(nx,p,lw,Black), w, blacken rw, cp))
    | _ -> bad_node()

  let fixup2 tp =
    let nw = third tp
    in if      is_red (right_child nw) then case4 tp
       else if is_red ( left_child nw) then case34 tp
       else case2 tp

  let rec fixup cx tp = 
    if cx = Red then (cx, node tp)
    else if is_black (third tp) then fixup2 tp
    else let (np1,w,rw) = case1 tp
         in let (cx2,np2) = fixup2 (lxrc np1)
         in fixup cx2 (np2,w,rw,Black)

end

module Right = struct

  let case1 = function
    | (Node(lw,w,rw,Red), p, nx, Black) -> (lw,w,Node(rw,p,nx,Red))
    | _ -> bad_node()

  let case2 (nw,p,nx,cp) = (cp , Node (redden nw,p,nx,Black))

  let case34 = function
    | (Node(lw,w,Node(lrw,xrw,rrw,Red),Black), p, nx, cp)
    -> (Red , Node (Node(lw,w,lrw,Black), xrw,
                    Node(rrw,p,nx,Black), cp))
    | _ -> bad_node()

  let case4 = function
    | (Node (lw,w,rw,Black),p,nx,cp) -> (Red , Node (blacken lw, w, Node(rw,p,nx,Black), cp))
    | _ -> bad_node()

  let fixup2 tp =
    let nw = first tp
    in if      is_red ( left_child nw) then case4 tp
       else if is_red (right_child nw) then case34 tp
       else case2 tp

  let rec fixup cx tp = 
    if cx = Red then (cx, node tp)
    else if is_black (first tp) then fixup2 tp
    else let (lw,w,np1) = case1 tp
         in let (cx2,np2) = fixup2 (lxrc np1)
         in fixup cx2 (lw,w,np2,Black)

end

let rec rem_min (lx,x,rx,cx) =
  match lxrc lx with
  | (Empty,y,ry,cy) -> (y, Left.fixup cy (ry,x,rx,cx))
  | tlx ->
    let (y,(cy,lxnew)) = rem_min tlx
    in (y, Left.fixup cy (lxnew,x,rx,cx))

module Remove = struct

  let remove (<<) (==) (>>) root x =
    let rec rem = function
      | (Empty,y,Empty,cy) -> if x==y then (cy,Empty) else raise Not_found
      | (ly, y, Empty, cy) -> let (cx,lx) = rem (lxrc ly) in  Left.fixup cx (lx,y,Empty,cy)
      | (Empty ,y, ry, cy) -> let (cx,rx) = rem (lxrc ry) in Right.fixup cx (Empty,y,rx,cy)
      | (ly,y,ry,cy) ->
        if x << y then let (cx,lynew) = match lxrc ly with
                         | (lx,y,Empty,cx) when x==y -> (cx,lx)
                         | (Empty,y,rx,cx) when x==y -> (cx,rx)
                         | tly -> rem tly
                       in Left.fixup cx (lynew,y,ry,cy)
        else
        if x >> y then let (cx,rynew) = match lxrc ry with
                         | (Empty,y,rx,cx) when x==y -> (cx,rx)
                         | (lx,y,Empty,cx) when x==y -> (cx,lx)
                         | t_ry -> rem t_ry
                       in Right.fixup cx (ly,y,rynew,cy)
        else match lxrc ry with
             | (Empty,xry,rry,cry) ->                       Right.fixup cry (ly,xry,rry,cy)
             | t_ry -> let (z,(cz,rynew)) = rem_min t_ry in Right.fixup cz  (ly,z,rynew,cy)
    in
    let new_root =
      match root with
      | Empty -> Empty
      | Node (left, y,Empty,_) when x==y -> left
      | Node (Empty,y,right,_) when x==y -> right
      | Node (l,y,r,c) -> try snd (rem (l,y,r,c)) with Not_found -> root
    in blacken new_root

end

let     remove     tree k = try             Remove.remove (<) (=) (>) tree k    with Not_found -> tree
let rec remove_all tree k = try remove_all (Remove.remove (<) (=) (>) tree k) k with Not_found -> tree


(***************************   functor   *******************************)

module type Typeof_Element =
  sig
    type t
    type tkey
    type tval
    val key: t -> tkey
    val value: t -> tval
    val compare: tkey -> tkey -> int
  end

module type Typeof_Make =
  functor (E: Typeof_Element) ->
    sig
      type element = E.t
      type nonrec t = element t
      val empty: t
      val size: t -> int
      val is_member: E.tkey -> t -> bool

      val find:  E.tkey -> t -> element option
      val value: E.tkey -> t -> E.tval  option

      val of_list: element list -> t
      val to_list: t -> element list

      val insert:     t -> element -> t
      val insert_new: t -> element -> t

      val remove:     t -> E.tkey -> t
      val remove_all: t -> E.tkey -> t

      val merge: t -> t -> t
      val union: t -> t -> t

      val fold_left:  ('a -> element -> 'a) -> 'a -> t -> 'a
      val fold_right: ('a -> element -> 'a) -> 'a -> t -> 'a

      val iter:         (element -> unit) -> t -> unit
      val iteri: (int -> element -> unit) -> t -> unit

    end

module Make: Typeof_Make =
  functor (E: Typeof_Element) -> struct

    type element = E.t

    let (<<<<) x y = E.compare (E.key x) (E.key y) < 0

    let (<<<) k y = E.compare k (E.key y) < 0
    let (===) k y = E.compare k (E.key y) = 0
    let (>>>) k y = E.compare k (E.key y) > 0

    type nonrec t = element t

    let empty = empty
    let size  = size
    let fold_left  = fold_left
    let fold_right = fold_right
    let iter  = iter
    let iteri = iteri
    let to_list = to_list

    let find k tree = 
      let rec look k ymino = function
          | Node (l,y,r,_) -> if k <<< y then look k  ymino   l
                              else            look k (Some y) r
          | Empty -> match ymino with
              | None -> None
              | Some y -> if k === y then Some y else None
      in match tree with
        | Empty -> None
        | Node (l,y,r,_) -> if k <<< y then look k   None   l
                            else            look k (Some y) r

    let is_member k tree = not (find k tree = None)

    let value k tree = (* find k tree >>= E.value *)
      match find k tree with
      | Some x -> Some (E.value x)
      | None -> None

    let insert tree x = Insert.insert (<<<<) tree x true
    let insert_new = Insert.insert_new (<<<<)

    let merge = Insert.union insert
    let union = Insert.union insert_new

    let of_list xs = List.fold_left insert Empty xs

    let remove_raw = Remove.remove (<<<) (===) (>>>)

    let     remove     tree key = try             remove_raw tree key      with Not_found -> tree
    let rec remove_all tree key = try remove_all (remove_raw tree key) key with Not_found -> tree

end
