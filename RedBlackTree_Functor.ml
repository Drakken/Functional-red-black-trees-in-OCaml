(*
 * a functional implementation of red-black trees
 * based on Cormen et. al., Introduction To Algorithms
 * copyright 2021 Daniel S. Bensen
 *)

module Make_RedBlackTree =
  functor (E: Element_Type) -> struct

    type element = E.t

    let key = E.key

    let (<<<) x y = E.compare x y < 0
    let (===) x y = E.compare x y = 0
    let (>>>) x y = E.compare x y > 0

    let (<<<<) x y = E.compare (key x) (key y) < 0
    let (>>>>) x y = E.compare (key x) (key y) > 0

    type color = Red | Black

    type t = Empty | Node of t * element * t * color

    let empty = Empty

    let rec mem k tree = match tree with
     | Empty -> false
     | Node (l,y,r,_) -> k === key y || (if k <<< key y then (mem k l) else (mem k r))

    let rec find k tree = match tree with
     | Empty -> None
     | Node (l,y,r,_) ->
         if      k <<< key y then find k l
         else if k >>> key y then find k r
         else Some y

    (* let value k tree = find k tree >>= E.value *)

    let rec map f = function
      | Empty -> Empty
      | Node(l,x,r,c) -> Node (map f l, f x, map f r, c)

    let rec fold f acc = function
      | Empty -> acc
      | Node (l,x,r,_) -> fold f (f (fold f acc l) x) r

    let rec size = function
      | Empty -> 0
      | Node (l,_,r,_) -> size l + 1 + size r

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
      in let (_,tree) = map 0 root
      in tree

    (*** insert ***)

    let is_red = function Node (_,_,_,Red) -> true | _ -> false

    let is_black node = not (is_red node)

    let not_found() = raise Not_found
    let already_in_tree() = invalid_arg "already in tree"

    let   bad_node () = invalid_arg "unexpected"
    let empty_node () = invalid_arg   "empty"

    let right_child = function Node (_,_,r,_) -> r | Empty -> empty_node()
    let  left_child = function Node (l,_,_,_) -> l | Empty -> empty_node()

    let blacken = function
      | Node (l,x,r,Red) -> Node (l,x,r,Black)
      | tree -> tree

    let insert tree x =
      let xnode = Node (Empty,x,Empty,Red)
      in
      let rec ins = function         (* rotations and recolorings are folded together in each case *)
      | Empty -> xnode               (*                see ascii art for details                   *)
      | Node (lz,z,rz,Black) -> 
        if x <<<< z                                                                  (* left side of black node *)
        then match lz with 
             | Node (ly,y,ry,Red) ->                                                          (* red child *)
               if x <<<< y
               then let lynew = ins ly in                                                 (* outer grandchild *)
                    if is_black lynew then Node(Node(lynew,y,ry,Red),z,rz,Black)
                    else match rz with
                         | Node (lu,u,ru,Red) -> Node(Node(lynew,y,ry,Black), z, Node(lu,u,ru,Black),  Red )
                         |           _        -> Node(         lynew,         y, Node(ry,z,rz, Red ), Black)
               else if x >>>> y
               then let rynew = ins ry in                                                 (* inner grandchild *)
                    match (rynew,rz) with
                    | Empty,_ | Node(_,_,_,Black),_ -> Node(Node(ly,y,rynew, Red ), z,            rz,       Black)
                    |    _ ,  Node (lu, u, ru,Red)  -> Node(Node(ly,y,rynew,Black), z,  Node(lu,u,ru,Black), Red )
                    |  Node (lx,xnew,rx,Red),  _    -> Node(Node(ly,y, lx,   Red ),xnew,Node(rx,z,rz, Red ),Black) 
               else already_in_tree()
             | _ -> Node (ins lz, z, rz, Black)
        else if x >>>> z                                                          (* right side of black node:  *)
        then match rz with                                                        (*  mirror image of left side *)
             | Node (ly,y,ry,Red) -> 
               if x >>>> y
               then let rynew = ins ry in
                    if is_black rynew then Node(lz,z,Node(ly,y,rynew,Red),Black)
                    else match lz with
                         | Node (lu,u,ru,Red)  -> Node(Node(lu, u, ru, Black), z,  Node(ly, y,rynew,Black), Red )
                         |          _          -> Node(Node(lz, z, ly,   Red ), y,  rynew,  Black)
               else if x <<<< y
               then let lynew = ins ly in
                    match (lynew,lz) with
                    | Empty,_ | Node(_,_,_,Black),_ -> Node(         lz,         z,  Node(lynew,y,ry, Red ),Black)
                    |     _    , Node (lu,u,ru,Red) -> Node(Node(lu,u,ru,Black), z,  Node(lynew,y,ry,Black), Red )
                    | Node (lx,xnew,rx,Red),   _    -> Node(Node(lz,z,lx, Red ),xnew,Node( rx,  y,ry, Red ),Black)
               else already_in_tree()
             | _ -> Node (lz, z, ins rz, Black)
        else already_in_tree()
      | _ -> invalid_arg "red child of red node"
      in
      blacken (ins (blacken tree))

    (*** delete ***)

    (* KEY: l = left child, r = right child, c = color, n = node, p = parent, t = tuple, x = element *)

    let  first (l,_,_,_) = l
    let  third (_,_,r,_) = r

    let node (l,x,r,c) = Node (l,x,r,c)

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
      let tlx = lxrc lx in
      match tlx with
      | (Empty,y,ry,cy) -> (y, Left.fixup cy (ry,x,rx,cx))
      |  _ ->
        let (y,(cy,lxnew)) = rem_min tlx
        in (y, Left.fixup cy (lxnew,x,rx,cx))

    let remove root k =
      let rec rem = function
        | (Empty,y,Empty,cy) -> if k === key y then (cy,Empty) else not_found()
        | (ly, y, Empty, cy) -> let (cx,lx) = rem (lxrc ly) in  Left.fixup cx (lx,y,Empty,cy)
        | (Empty ,y, ry, cy) -> let (cx,rx) = rem (lxrc ry) in Right.fixup cx (Empty,y,rx,cy)
        | (ly,y,ry,cy) ->
          if k <<< key y
          then let tly = lxrc ly in
               let (cx,lynew) = match tly with
                 | (lx,x,Empty,cx) when k === key x -> (cx,lx)
                 | (Empty,x,rx,cx) when k === key x -> (cx,rx)
                 | _ -> rem tly
               in Left.fixup cx (lynew,y,ry,cy)
          else
          let t_ry = lxrc ry in
          if k >>> key y
          then let (cx,rynew) = match t_ry with
                 | (Empty,x,rx,cx) when k === key x -> (cx,rx)
                 | (lx,x,Empty,cx) when k === key x -> (cx,lx)
                 | _ -> rem t_ry
               in Right.fixup cx (ly,y,rynew,cy)
          else match t_ry with
               | (Empty,xry,rry,cry) ->                    Right.fixup cry (ly,xry,rry,cy)
               | _ -> let (z,(cz,rynew)) = rem_min t_ry in Right.fixup cz  (ly,z,rynew,cy)
      in
      let new_root =
        match root with
        | Empty -> not_found()
        | Node (left, x,Empty,_) when k === key x -> left
        | Node (Empty,x,right,_) when k === key x -> right
        | _ -> let (_,nnew) = rem (lxrc root) in nnew
        in blacken new_root

    let remove_if_in tree k = try remove tree k with Not_found -> tree
  
end


