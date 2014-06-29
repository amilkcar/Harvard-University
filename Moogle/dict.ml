(* Interfaces and implementations of dictionaries.  A dictionary
 * is used to associate a value with a key.  In our case, we will
 * be using a dictionary to build an index for the web, associating
 * a set of URLs with each word that we find as we crawl the web.
 *)
exception TODO
exception NOT_VALID_EQUAL

module type DICT = 
sig
  type key   
  type value 
  type dict

  (* An empty dictionary *)
  val empty : dict 

  (* Reduce the dictionary using the provided function f and base case u. 
   * Our reducing function f must have the type:
   *      key -> value -> 'a -> 'a
   * and our base case u has type 'a.
   * 
   * If our dictionary is the (key,value) pairs (in any order)
   *      (k1,v1), (k2,v2), (k3,v3), ... (kn,vn)
   * then fold should return:
   *      f k1 v1 (f k2 v2 (f k3 v3 (f ... (f kn vn u))))
   *)
  val fold : (key -> value -> 'a -> 'a) -> 'a -> dict -> 'a

  (* Returns as an option the value associated with the provided key. If
   * the key is not in the dictionary, return None. *)
  val lookup : dict -> key -> value option

  (* Returns true if and only if the key is in the dictionary. *)
  val member : dict -> key -> bool

  (* Inserts a (key,value) pair into our dictionary. If the key is already
   * in our dictionary, update the key to have the new value. *)
  val insert : dict -> key -> value -> dict

  (* Removes the given key from the dictionary. If the key is not present,
   * return the original dictionary. *)
  val remove : dict -> key -> dict

  (* Return an arbitrary key, value pair along with a new dict with that
   * pair removed. Return None if the input dict is empty *)
  val choose : dict -> (key * value * dict) option

  (* functions to convert our types to strings for debugging and logging *)
  val string_of_key: key -> string
  val string_of_value : value -> string
  val string_of_dict : dict -> string

  (* Runs all the tests. see TESTING EXPLANATION below *)
  val run_tests : unit -> unit
end



(* Argument module signature to our DICT functors *)
module type DICT_ARG =
sig
  type key
  type value
  val compare : key -> key -> Order.order
  val string_of_key : key -> string
  val string_of_value : value -> string

  (* Use these functions for testing. See TESTING EXPLANATION. *)

  (* Generate a key. The same key is always returned *)
  val gen_key : unit -> key

  (* Generate a random key. *)
  val gen_key_random : unit -> key

  (* Generates a key greater than the argument. *)
  val gen_key_gt : key -> unit -> key

  (* Generates a key less than the argument. *)
  val gen_key_lt : key -> unit -> key

  (* Generates a key between the two arguments. Return None if no such
   * key exists. *)
  val gen_key_between : key -> key -> unit -> key option

  (* Generates a random value. *)
  val gen_value : unit -> value

  (* Generates a random (key,value) pair *)
  val gen_pair : unit -> key * value
end



(* An example implementation of our DICT_ARG signature. Use this struct
 * for testing. *)
module IntStringDictArg : DICT_ARG =
struct
  open Order
  type key = int
  type value = string

  let compare x y = if x < y then Less else if x > y then Greater else Eq
  let string_of_key = string_of_int
  let string_of_value v = v
  let gen_key () = 0
  let gen_key_gt x () = x + 1
  let gen_key_lt x () = x - 1
  let gen_key_between x y () = 
    let (lower, higher) = (min x y, max x y) in
    if higher - lower < 2 then None else Some (higher - 1)
  let gen_key_random =
    let _ = Random.self_init () in
    (fun () -> Random.int 10000)

  (* returns the nth string in lst, or "cow" n > length of list *)
  let rec lst_n (lst: string list) (n: int) : string =
    match lst with
      | [] -> "cow"
      | hd::tl -> if n = 0 then hd else lst_n tl (n-1)

  (* list of possible values to generate *)
  let possible_values = ["a";"c";"d";"e";"f";"g";"h";"i";"j";"k";"m";"n";
                         "o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z";
                         "zzzzzz";"cheese";"foo";"bar";"baz";"quux";"42"]
  let num_values = List.length possible_values
  (* gen_value will return the string at this current index *)
  let current_index = ref 0
  let gen_value () =
    let index = !current_index in
    if index >= num_values then
      (current_index := 0; lst_n possible_values index)
    else
      (current_index := index + 1; lst_n possible_values index)
  let gen_pair () = (gen_key_random(), gen_value())
end



(* An association list implementation of our DICT signature. *)
module AssocListDict(D:DICT_ARG) : (DICT with type key = D.key
  with type value = D.value) = 
struct
  open Order;;
  type key = D.key;;
  type value = D.value;;
  type dict = (key * value) list;;

  (* INVARIANT: sorted by key, no duplicates *)

  let empty = [] ;;

  let fold f d = List.fold_left (fun a (k,v) -> f k v a) d 

  let rec lookup d k = 
    match d with 
      | [] -> None
      | (k1,v1)::d1 -> 
        (match D.compare k k1 with
          | Eq -> Some v1
          | Greater -> lookup d1 k 
          | _ -> None
        )

  let member d k = 
    match lookup d k with 
      | None -> false 
      | Some _ -> true

  let rec insert d k v = 
    match d with 
      | [] -> [(k,v)]
      | (k1,v1)::d1 -> 
        (match D.compare k k1 with 
          | Less -> (k,v)::d
          | Eq -> (k,v)::d1
          | Greater -> (k1,v1)::(insert d1 k v)
        )

  let rec remove d k = 
    match d with 
      | [] -> []
      | (k1,v1)::d1 ->
        (match D.compare k k1 with 
          | Eq -> d1
          | Greater -> (k1,v1)::(remove d1 k)
          | _ -> d
        )
      
  let choose d = 
    match d with 
      | [] -> None
      | (k,v)::rest -> Some(k,v,rest)

  let string_of_key = D.string_of_key

  let string_of_value = D.string_of_value

  let string_of_dict (d: dict) : string = 
    let f = (fun y (k,v) -> y ^ "\n key: " ^ D.string_of_key k ^ 
      "; value: (" ^ D.string_of_value v ^ ")") in
      List.fold_left f "" d

  (****************************************************************)
  (* Tests for our AssocListDict functor                          *)
  (* These are just examples of tests, your tests should be a lot *)
  (* more thorough than these.                                    *)
  (****************************************************************)

  (* adds a list of (key,value) pairs in left-to-right order *)
  let insert_list (d: dict) (lst: (key * value) list) : dict = 
    List.fold_left (fun r (k,v) -> insert r k v) d lst

  (* adds a list of (key,value) pairs in right-to-left order *)
  let insert_list_reversed (d: dict) (lst: (key * value) list) : dict =
    List.fold_right (fun (k,v) r -> insert r k v) lst d

  (* generates a (key,value) list with n distinct keys in increasing order *)
  let generate_pair_list (size: int) : (key * value) list =
    let rec helper (size: int) (current: key) : (key * value) list =
      if size <= 0 then []
      else 
        let new_current = D.gen_key_gt current () in
        (new_current, D.gen_value()) :: (helper (size - 1) new_current)
    in
    helper size (D.gen_key ())

  (* generates a (key,value) list with keys in random order *)
  let rec generate_random_list (size: int) : (key * value) list =
    if size <= 0 then []
    else 
      (D.gen_key_random(), D.gen_value()) :: (generate_random_list (size - 1))

  let test_insert () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list empty pairs1 in
    List.iter (fun (k,v) -> assert(lookup d1 k = Some v)) pairs1 ;
    ()

  let test_remove () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list empty pairs1 in
    List.iter 
      (fun (k,v) -> 
        let r = remove d1 k in
        List.iter 
          (fun (k2,v2) ->
            if k = k2 then assert(lookup r k2 = None)
            else assert(lookup r k2 = Some v2)
          ) pairs1
      ) pairs1 ;
    ()

  let test_lookup () =
    ()

  let test_choose () =
    ()

  let test_member () =
    ()

  let test_fold () =
    ()

  let run_tests () = 
    test_insert() ;
    test_remove() ;
    test_lookup() ;
    test_choose() ;
    test_member() ;
    test_fold() ;
    ()

end    



(******************************************************************)
(* BTDict: a functor that implements our DICT signature           *)
(* using a balanced tree (2-3 trees)                              *)
(******************************************************************)

module BTDict(D:DICT_ARG) : (DICT with type key = D.key
with type value = D.value) =
struct
  open Order

  exception TODO

  type key = D.key
  type value = D.value

  (* A dictionary entry is a (key,value) pair. We compare two (key,value)
   * pairs with the provided key-comparison function D.compare. For example,
   * we may choose to keep a dictionary mapping links to their ranks. In this
   * case, our (key,value) pairs will be (link,rank) pairs, and we compare
   * links using string comparison. *)
  type pair = key * value

  (* Type definition for dictionary,which we choose to represent as a 2-3 Tree.
   * This is almost the same as the binary search tree definition from pset4 &
   * lecture, except we add one more case: a Three-node. 
   *
   * A Three-node contains two pairs and three subtrees: left, middle, and 
   * right, represented by the 3 dicts in the definition below. *)
  type dict = 
    | Leaf
    | Two of dict * pair * dict
    | Three of dict * pair * dict * pair * dict

  (* INVARIANTS: 
   * 2-node: Two(left,(k1,v1),right) 
   * (1) Every key k appearing in subtree left must be k < k1.
   * (2) Every key k appearing in subtree right must be k > k1. 
   * (3) The length of the path from the 2-node to
   *     every leaf in its two subtrees must be the same.  
   * 
   * 3-node: Three(left,(k1,v1),middle,(k2,v2),right) 
   * (1) k1 < k2.
   * (2) Every key k appearing in subtree left must be k < k1. 
   * (3) Every key k appearing in subtree right must be k > k2. 
   * (4) Every key k appearing in subtree middle must be k1 < k < k2.
   * (5) The length of the path from the 3-node to every leaf in its three 
   *     subtrees must be the same. 
   *)

  (* FOR INSERTION:
   * A kicked configuration returned by going downwards on insertion.
   * We can only kick up Two nodes, hence Up takes a dict * pair * dict *)
  type kicked =
    | Up of dict * pair * dict
    | Done of dict

  (* FOR REMOVAL:
   * A hole configuration returned by going downwards on removal. We
   * include a pair option whenever we remove the minimum of the right
   * subtree of the current pair in order the current pair *)
  type hole =
    | Hole of pair option * dict
    | Absorbed of pair option * dict

  (* FOR REMOVAL:
   * A direction will distinguish which configuration we came from in the
   * removal cases. We use direction2 for cases (1-2) on the handout, and
   * we use direction3 for cases (3-4) on the handout. *)
  type direction2 =
    | Left2
    | Right2

  type direction3 =
    | Left3
    | Mid3
    | Right3
        
  (* How do we represent an empty dictionary with 2-3 trees? *)
  let empty : dict = Leaf

  (* TODO:
   * Implement fold. Read the specification in the DICT signature above. *)
  let rec fold (f: key -> value -> 'a -> 'a) (u: 'a) (d: dict) : 'a =
    match d with 
    | Leaf -> u
    | Two(l, (k,v), r) ->
      let u = f k v u in
      fold f (fold f u r) l 
    | Three(l, (k1, v1), m, (k2, v2), r) ->
      let u1 = f k1 v1 u in 
      let u2 = f k2 v2 u1 in
      fold f (fold f (fold f u2 r) m) l

  (* TODO:
   * Implement these to-string functions *)
  let string_of_key = D.string_of_key
  let string_of_value = D.string_of_value
  let string_of_dict (d: dict) : string = 
    let f = (fun k v y -> y ^ "\n key: " ^ D.string_of_key k ^ 
      "; value: (" ^ D.string_of_value v ^ ")") in
    fold f "" d
      
  (* Debugging function. This will print out the tree in text format.
   * Use this function to see the actual structure of your 2-3 tree. *
   *
   * e.g.      (4,d)   (6,f)
   *         /       |       \
   *      (2,b)    (4,d)     Leaf
   *      /  \     /   \
   *   Leaf  Leaf Leaf  Leaf
   *
   * string_of_tree will output:
   * Three(Two(Leaf,(2,b),Leaf),(4,d),Two(Leaf,(5,e),Leaf),(6,f),Leaf)
   *
   * Note that this tree is NOT balanced, because all the paths from (6,f)
   * to its leaves do NOT all have the same length. *)
  let rec string_of_tree (d: dict) : string = 
    match d with
      | Leaf -> "Leaf"
      | Two(left,(k,v),right) -> "Two(" ^ (string_of_tree left) 
        ^ ",(" ^ (string_of_key k) ^ "," ^ (string_of_value v) ^ "),"
        ^ (string_of_tree right) ^ ")"
      | Three(left,(k1,v1),middle,(k2,v2),right) -> 
        "Three(" ^ (string_of_tree left)
        ^ ",(" ^ (string_of_key k1) ^ "," ^ (string_of_value v1) ^ "),"
        ^ (string_of_tree middle) ^ ",(" ^ (string_of_key k2) ^ "," 
        ^ (string_of_value v2) ^ ")," ^ (string_of_tree right) ^ ")"

  (* Upward phase for w where its parent is a Two node whose (key,value) is x.
   * One of x's children is w, and the other child is x_other. This function
   * should return a kicked-up configuration containing the new tree as a
   * result of performing the upward phase on w. *)
  let insert_upward_two (w: pair) (w_left: dict) (w_right: dict) 
      (x: pair) (x_other: dict) : kicked = 
      match w,x with
        | (k1,v1),(k2,v2) -> 
          (match D.compare k1 k2  with
             | Eq -> raise NOT_VALID_EQUAL 
             | Less -> Done (Three(w_left,w,w_right,x,x_other)) 
             | Greater -> Done (Three(x_other,x,w_left,w,w_right)) 
          )

  (* Upward phase for w where its parent is a Three node whose (key,value) is x
   * One of x's children is w, and of the two remaining children, 
   * other_left is the subtree more to the left and other_right is the 
   * subtree more to the right. 
   *
   * E.g. From our handout,for the first case where w's parent is a Three-tree,
   * other_left would be c and other_right would be d. For the second case,
   * other_left would be a and other_right would be d. For the third case,
   * other_left would be a and other_right would be b. 
   *
   * This function should return a kicked-up configuration containing the 
   * new tree as a result of performing the upward phase on w. *)
  let insert_upward_three (w: pair) (w_left: dict) (w_right: dict)
      (x: pair) (y: pair) (other_left: dict) (other_right: dict) : kicked =
      match w,x,y with
        | (kw,_),(kx,vx),(ky,_) -> 
          (match (D.compare kw kx),(D.compare kw ky) with
               | Eq,_ -> raise NOT_VALID_EQUAL
               | Less,_ -> 
                   Up(Two(w_left,w,w_right),x,Two(other_left,y,other_right))
               | Greater,Eq -> raise NOT_VALID_EQUAL       
               | Greater,Less -> Up(Two(other_left,x,w_left),w,
                                    Two(w_right,y,other_right)) 
               | Greater,Greater -> Up(Two(other_left,x,other_right),y,
                                       Two(w_left,w,w_right))                 
          )      

  (* Downward phase for inserting (k,v) into our dictionary d. 
   * The downward phase returns a "kicked" up configuration, where
   * 
   * type kicked =
   *      | Up of dict * pair * dict
   *      | Done of dict
   * 
   * A kicked up configuration can only be a Two node, hence the Up
   * constructor takes the same parameters as the Two constructor. We return
   * Up(left,(k,v),right) if the Two-node represented by this Up needs to
   * be further kicked up in the upward phase (this is represented by an up
   * arrow on the 2-3 Tree handout). We return Done(d) if we have finished
   * our upward phase on the tree represented by d. 
   *
   * The functions insert_downward, insert_downward_two, and 
   * insert_downward_three are __mutually recursive__, hence the 
   * "let rec" and the "and" keywords. Here, we use three mutually recursive
   * functions to simplify our code into smaller pieces.
   *
   * Two functions f and g are __mutually recursive__ if in f's definition, 
   * f calls g, and in g's definition, g calls f. This definition of
   * mutually recursive definitions can be extended to more than two functions,
   * as follows: 
   * 
   * Functions f1, f2, f3, ..., fn are mutually recursive if for each of
   * these functions f, all of the other f_i's can be called on some execution 
   * of f. *)

  (* insert_downward should handle the base case when inserting into a Leaf,
   * and if our dictionary d is a Two-node or a Three-node, we call the 
   * corresponding functions insert_downward_two or insert_downward_three
   * with the appropriate arguments. *)
  let rec insert_downward (d: dict) (k: key) (v: value) : kicked =
    match d with
      | Leaf -> Up(Leaf,(k,v),Leaf)      
      | Two(left,x,right) -> insert_downward_two (k,v) x left right 
      | Three(left,x,middle,y,right) -> 
        insert_downward_three (k,v) x y left middle right  


  (* Downward phase on a Two node. (k,v) is the (key,value) we are inserting,
   * (k1,v1) is the (key,value) of the current Two node, and left and right
   * are the two subtrees of the current Two node. *)
  and insert_downward_two ((k,v): pair) ((k1,v1): pair) 
      (left: dict) (right: dict) : kicked = 
        match D.compare k k1 with
          | Eq -> Done(Two(left, (k, v), right)) 
          | Less -> 
            (match insert_downward left k v with 
              | Up(m,w,r) -> insert_upward_two w m r (k1,v1) right  
              | Done a -> Done (Two(a,(k1,v1),right))
            )
          | Greater -> 
            (match insert_downward right k v with 
              | Up(m,w,r) -> insert_upward_two w m r (k1,v1) left      
              | Done a -> Done (Two(left,(k1,v1),a))
            )

  (* Downward phase on a Three node. (k,v) is the (key,value) we are inserting,
   * (k1,v1) and (k2,v2) are the two (key,value) pairs in our Three node, and
   * left,middle, and right are the three subtrees of our current Three node *)
  and insert_downward_three ((k,v): pair) ((k1,v1): pair) ((k2,v2): pair) 
      (left: dict) (middle: dict) (right: dict) : kicked =
      match D.compare k k1 with
        | Eq -> Done (Three(left,(k,v),middle,(k2,v2),right)) 
        | Less -> 
          (match insert_downward left k v with 
            | Up(m,w,r) -> 
              insert_upward_three w m r (k1,v1) (k2,v2) middle right     
            | Done a -> Done (Three(a,(k1,v1),middle,(k2,v2),right))
          )
        | Greater -> 
          (match D.compare k k2 with
            | Eq -> Done(Three( left, (k1,v1) ,middle, (k,v) ,right))
            | Less ->  
                (match insert_downward middle k v with
                  | Up(m,w,r) -> 
                    insert_upward_three w m r (k1,v1) (k2,v2) left right
                  | Done a -> Done (Three(left,(k1,v1),a,(k2,v2),right))   
                )           
            | Greater ->  
                (match insert_downward right k v with 
                  | Up(m,w,r) -> 
                    insert_upward_three  w m r (k1,v1) (k2,v2) left middle
                  | Done a -> Done(Three(left,(k1,v1),middle,(k2,v2),a))   
                ) 
          )   


  (* We insert (k,v) into our dict using insert_downward, which gives us
   * "kicked" up configuration. We return the tree contained in the "kicked"
   * configuration. *)
  let insert (d: dict) (k: key) (v: value) : dict =
    match insert_downward d k v with
      | Up(l,(k1,v1),r) -> Two(l,(k1,v1),r)
      | Done x -> x

  (* Upward phase for removal where the parent of the hole is a Two node. 
   * See cases (1-2) on the handout. n is the (key,value) pair contained in
   * the parent node; left and right are the subtrees of the parent node (our
   * hole is one of these subtrees); and dir indicates which subtree was
   * contained by the hole. *)
  let remove_upward_two (n: pair) (rem: pair option) 
      (left: dict) (right: dict) (dir: direction2) : hole =
    match dir,n,left,right with
      | Left2,x,l,Two(m,y,r) -> Hole(rem,Three(l,x,m,y,r))
      | Right2,y,Two(l,x,m),r -> Hole(rem,Three(l,x,m,y,r))
      | Left2,x,a,Three(b,y,c,z,d) -> 
        Absorbed(rem,Two(Two(a,x,b),y,Two(c,z,d)))
      | Right2,z,Three(a,x,b,y,c),d -> 
        Absorbed(rem,Two(Two(a,x,b),y,Two(c,z,d)))
      | Left2,_,_,_ | Right2,_,_,_ -> Absorbed(rem,Two(Leaf,n,Leaf))

  (* Upward phase for removal where the parent of the hole is a Three node.
   * See cases (3-4) on the handout. n1 and n2 are the (key,value) pairs
   * contained in the parent node; left, middle, and right are the subtrees
   * of the parent node (our hole is one of these subtrees); and dir indicates
   * which subtree was the tree contained by the hole. *)
  let remove_upward_three (n1: pair) (n2: pair) (rem: pair option)
      (left: dict) (middle: dict) (right: dict) (dir: direction3) : hole =
    match dir,n1,n2,left,middle,right with
      | Left3,x,z,a,Two(b,y,c),d -> Absorbed(rem,Two(Three(a,x,b,y,c),z,d))
      | Mid3,y,z,Two(a,x,b),c,d -> Absorbed(rem,Two(Three(a,x,b,y,c),z,d))
      | Mid3,x,y,a,b,Two(c,z,d) -> Absorbed(rem,Two(a,x,Three(b,y,c,z,d)))
      | Right3,x,z,a,Two(b,y,c),d -> Absorbed(rem,Two(a,x,Three(b,y,c,z,d)))
      | Left3,w,z,a,Three(b,x,c,y,d),e -> 
        Absorbed(rem,Three(Two(a,w,b),x,Two(c,y,d),z,e))
      | Mid3,y,z,Three(a,w,b,x,c),d,e -> 
        Absorbed(rem,Three(Two(a,w,b),x,Two(c,y,d),z,e))
      | Mid3,w,x,a,b,Three(c,y,d,z,e) -> 
        Absorbed(rem,Three(a,w,Two(b,x,c),y,Two(d,z,e)))
      | Right3,w,z,a,Three(b,x,c,y,d),e -> 
        Absorbed(rem,Three(a,w,Two(b,x,c),y,Two(d,z,e)))
      | Left3,_,_,_,_,_ | Mid3,_,_,_,_,_ | Right3,_,_,_,_,_ ->
        Absorbed(rem,Three(Leaf,n1,Leaf,n2,Leaf))

  (* DO NOT EDIT THIS *)
  let rec remove_downward (d: dict) (k: key) : hole =
    match d with
      | Leaf -> Absorbed(None,d)
      | Two(Leaf,(k1,v1),Leaf) ->
        (match D.compare k k1 with
          | Eq -> Hole(Some(k1,v1),Leaf)
          | Less | Greater -> Absorbed(None,d)
        )
      | Three(Leaf,(k1,v1),Leaf,(k2,v2),Leaf) ->
        (match D.compare k k1, D.compare k k2 with
          | Eq, _ -> Absorbed(Some(k1,v1),Two(Leaf,(k2,v2),Leaf))
          | _, Eq -> Absorbed(Some(k2,v2),Two(Leaf,(k1,v1),Leaf))
          | _, _ -> Absorbed(None,d)
        )
      | Two(l,n,r) -> remove_downward_two k n l r
      | Three(l,n1,m,n2,r) -> remove_downward_three k n1 n2 l m r

  (* DO NOT EDIT THIS *)
  and remove_downward_two (k: key) ((k1,v1): pair) 
      (left: dict) (right: dict) : hole =
    match D.compare k k1 with
      | Eq ->
        (match remove_min right with
          | Hole(None,_) -> Hole(None,left)
          | Hole(Some n,new_right) -> 
            remove_upward_two n None left new_right Right2
          | Absorbed(None,_) -> Hole(None,left)
          | Absorbed(Some n,new_right) -> Absorbed(None,Two(left,n,new_right))
        )
      | Less -> 
        (match remove_downward left k with
          | Hole(rem,t) -> remove_upward_two (k1,v1) rem t right Left2
          | Absorbed(rem,t) -> Absorbed(rem,Two(t,(k1,v1),right))
        )
      | Greater ->
        (match remove_downward right k with
          | Hole(rem,t) -> remove_upward_two (k1,v1) rem left t Right2
          | Absorbed(rem,t) -> Absorbed(rem,Two(left,(k1,v1),t))
        )

  (* DO NOT EDIT THIS *)
  and remove_downward_three (k: key) ((k1,v1): pair) ((k2,v2): pair)
      (left: dict) (middle: dict) (right: dict) : hole =
    match D.compare k k1, D.compare k k2 with
      | Eq, _ ->
        (match remove_min middle with
          | Hole(None,_) -> Hole(None,Two(left,(k2,v2),right))
          | Hole(Some n,new_middle) -> 
            remove_upward_three n (k2,v2) None left new_middle right Mid3
          | Absorbed(None,_) -> Absorbed(None,Two(left,(k1,v1),right))
          | Absorbed(Some n,new_middle) -> 
            Absorbed(None,Three(left,n,new_middle,(k2,v2),right))
        )
      | _ , Eq ->
        (match remove_min right with
          | Hole(None,_) -> Hole(None,Two(left,(k1,v1),middle))
          | Hole(Some n,new_right) -> 
            remove_upward_three (k1,v1) n None left middle new_right Right3
          | Absorbed(None,_) -> Absorbed(None,Two(left,(k1,v1),middle))
          | Absorbed(Some n,new_right) -> 
            Absorbed(None,Three(left,(k1,v1),middle,n,new_right))
        )
      | Less, _ ->
        (match remove_downward left k with
          | Hole(rem,t) -> 
            remove_upward_three (k1,v1) (k2,v2) rem t middle right Left3
          | Absorbed(rem,t) -> 
            Absorbed(rem,Three(t,(k1,v1),middle,(k2,v2),right))
        )
      | _, Greater ->
        (match remove_downward right k with
          | Hole(rem,t) -> 
            remove_upward_three (k1,v1) (k2,v2) rem left middle t Right3
          | Absorbed(rem,t) -> 
            Absorbed(rem,Three(left,(k1,v1),middle,(k2,v2),t))
        )
      | Greater, Less ->
        (match remove_downward middle k with
          | Hole(rem,t) -> 
            remove_upward_three (k1,v1) (k2,v2) rem left t right Mid3
          | Absorbed(rem,t) -> 
            Absorbed(rem,Three(left,(k1,v1),t,(k2,v2),right))
        )

  (* DO NOT EDIT THIS *)
  and remove_min (d: dict) : hole =
    match d with
      | Leaf -> Hole(None,Leaf)
      | Two(Leaf,n,_) -> Hole(Some n,Leaf)
      | Three(Leaf,n1,middle,n2,right) -> 
    Absorbed(Some n1,Two(middle,n2,right))
      | Two(left,n,right) -> 
        (match remove_min left with
          | Hole(rem,t) -> remove_upward_two n rem t right Left2
          | Absorbed(rem,t) -> Absorbed(rem,Two(t,n,right))
        )
      | Three(left,n1,middle,n2,right) ->
        (match remove_min left with
          | Hole(rem,t) -> 
        remove_upward_three n1 n2 rem t middle right Left3
          | Absorbed(rem,t) -> 
        Absorbed(rem,Three(t,n1,middle,n2,right))
        )

  (* DO NOT EDIT THIS *)
  let remove (d: dict) (k: key) : dict =
    match remove_downward d k with
      | Hole(_,d') -> d'
      | Absorbed(_,d') -> d'

  (* TODO:
   * Write a lookup function that returns the value of the given key
   * in our dictionary and returns it as an option, or return None
   * if the key is not in our dictionary. *)
  let rec lookup (d: dict) (k: key) : value option =
    match d with 
    | Leaf -> None
    | Two(l, (nk,nv), r) -> 
      (match D.compare k nk with 
      | Eq -> Some nv
      | Greater -> lookup r k
      | Less -> lookup l k)
    | Three(l, (nk1, nv1), m, (nk2, nv2), r) ->
      (match D.compare k nk1 with
      | Eq -> Some nv1
      | Less -> lookup l k
      | Greater -> (
    match D.compare k nk2 with 
    | Eq -> Some nv2
    | Less -> lookup m k
    | Greater -> lookup r k
      ))

  (* TODO:
   * Write a function to test if a given key is in our dictionary *)
  let member (d: dict) (k: key) : bool =
    match lookup d k with 
    | Some _ -> true
    | None -> false

  (* TODO:
   * Write a function that removes any (key,value) pair from our 
   * dictionary (your choice on which one to remove), and returns
   * as an option this (key,value) pair along with the new dictionary. 
   * If our dictionary is empty, this should return None. *)
  let choose (d: dict) : (key * value * dict) option =
    match d with 
    | Leaf -> None
    | Two(_, (k,v), _) -> Some (k,v,remove d k)
    | Three(_, (k,v), _, _, _) -> Some (k,v,remove d k)

  (* TODO:
   * Write a function that when given a 2-3 tree (represented by our
   * dictionary d), returns true if and only if the tree is "balanced", 
   * where balanced means that the given tree satisfies the 2-3 tree
   * invariants stated above and in the 2-3 tree handout. *)

  (* How are you testing that you tree is balanced? 
   * ANSWER: 
   *    Calculate the height of the tree recursively.
   *    If subtrees are of a different height, height returns None
   *    For each tree level, add 1 to the height
   *    If subtrees are of an equal height recursively(for each 2 & 3 node), 
   *    then the tree is balanced
   *)
  let rec height (d: dict) : int option = 
    match d with
    | Leaf -> Some 0
    | Two(l, _, r) -> 
      let height_l = height l in
      (match height_l with 
      | None -> None
      | Some h -> 
    if height_l = (height r) 
    then Some (h + 1) else None)
    | Three(l, _, m, _, r) -> 
      let height_l = height l in
      (match height_l with
      | None -> None
      | Some h -> 
    if height_l = (height m) && height_l = (height r) 
    then Some (h + 1) else None)

  let balanced (d: dict) : bool =
    match height d with 
      | Some h -> true
      | None -> false


  (********************************************************************)
  (*       TESTS                                                      *)
  (* You must write more comprehensive tests, using our remove tests  *)
  (* below as an example                                              *)
  (********************************************************************)

  (* adds a list of (key,value) pairs in left-to-right order *)
  let insert_list (d: dict) (lst: (key * value) list) : dict = 
    List.fold_left (fun r (k,v) -> 
      insert r k v) d lst

  (* adds a list of (key,value) pairs in right-to-left order *)
  let insert_list_reversed (d: dict) (lst: (key * value) list) : dict =
    List.fold_right (fun (k,v) r -> insert r k v) lst d

  (* generates a (key,value) list with n distinct keys in increasing order *)
  let generate_pair_list (size: int) : (key * value) list =
    let rec helper (size: int) (current: key) : (key * value) list =
      if size <= 0 then []
      else 
        let new_current = D.gen_key_gt current () in
        (new_current, D.gen_value()) :: (helper (size - 1) new_current)
    in
    helper size (D.gen_key ())

  (* generates a (key,value) list with keys in random order *)
  let rec generate_random_list (size: int) : (key * value) list =
    if size <= 0 then []
    else 
      (D.gen_key_random(), D.gen_value()) :: (generate_random_list (size - 1))


  let test_balance () =
    let d1 = Leaf in
    assert(balanced d1) ;

    let d2 = Two(Leaf,D.gen_pair(),Leaf) in
    assert(balanced d2) ;

    let d3 = Three(Leaf,D.gen_pair(),Leaf,D.gen_pair(),Leaf) in
    assert(balanced d3) ;

    let d4 = Three(Two(Two(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                           Two(Leaf,D.gen_pair(),Leaf)),
                       D.gen_pair(),Two(Two(Leaf,D.gen_pair(),Leaf),
                                        D.gen_pair(),
                                        Two(Leaf,D.gen_pair(),Leaf))),
                   D.gen_pair(),
                   Two(Two(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                           Two(Leaf,D.gen_pair(),Leaf)),D.gen_pair(),
                       Two(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                           Two(Leaf,D.gen_pair(),Leaf))),D.gen_pair(),
                   Two(Two(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                           Two(Leaf,D.gen_pair(),Leaf)),D.gen_pair(),
                       Three(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                             Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                             Three(Leaf,D.gen_pair(),Leaf,D.gen_pair(),Leaf))))
    in
    assert(balanced d4) ;

    let d5 = Two(Leaf,D.gen_pair(),Two(Leaf,D.gen_pair(),Leaf)) in
    assert(not (balanced d5)) ;

    let d6 = Three(Leaf,D.gen_pair(),
                   Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),Leaf) in
    assert(not (balanced d6)) ;

    let d7 = Three(Three(Leaf,D.gen_pair(),Leaf,D.gen_pair(),Leaf),
                   D.gen_pair(),Leaf,D.gen_pair(),Two(Leaf,D.gen_pair(),Leaf))
    in
    assert(not (balanced d7)) ;
    () 

  let test_lookup_choose () = 
    let base_key = D.gen_key () in
    let p1 = (base_key, D.gen_value ()) in
    let p2 = (D.gen_key_gt (fst p1) (), D.gen_value ()) in
    let p3 = (D.gen_key_gt (fst p2) (), D.gen_value ()) in 
    let p4 = (D.gen_key_gt (fst p3) (), D.gen_value ()) in
    let p5 = (D.gen_key_gt (fst p4) (), D.gen_value ()) in
    let p6 = (D.gen_key_gt (fst p5) (), D.gen_value ()) in
    let p7 = (D.gen_key_gt (fst p6) (), D.gen_value ()) in
    let p8 = (D.gen_key_gt (fst p7) (), D.gen_value ()) in
    let p9 = (D.gen_key_gt (fst p8) (), D.gen_value ()) in
    let p10 = (D.gen_key_gt (fst p9) (), D.gen_value ()) in
    let p11 = (D.gen_key_gt (fst p10) (), D.gen_value ()) in
    let p12 = (D.gen_key_gt (fst p11) (), D.gen_value ()) in
    let d1 = 
      Three(
    Two(
      Two(Leaf,p1,Leaf),
      p2,
      Two(Leaf,p3,Leaf)
    ),
    p4,
    Two(
      Two(Leaf,p5,Leaf),
      p6,
      Two(Leaf,p7,Leaf)
    ),
    p8,
    Two(
      Two(Leaf,p9,Leaf),
      p10,
      Two(Leaf,p11,Leaf)
    )
      ) in
    let members = [p1;p2;p3;p4;p5;p6;p7;p8;p9;p10;p11] in
    List.iter (fun x -> assert (lookup d1 (fst x) = Some (snd x))) members;
    assert (lookup d1 (fst p12) = None);
    List.iter (fun x -> assert (member d1 (fst x))) members;
    assert (not (member d1 (fst p12)));
    assert ((List.fold_right (fun x da -> match (choose da) with 
    | None -> assert (false)
    | Some(k, v, d) -> d) members d1) = empty);
    ()


  let test_remove_nothing () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list empty pairs1 in
    let r2 = remove d1 (D.gen_key_lt (D.gen_key()) ()) in
    List.iter (fun (k,v) -> 
      assert(lookup r2 k = Some v)) pairs1 ;
    assert(balanced r2) ;
    ()

  let test_remove_from_nothing () =
    let d1 = empty in
    let r1 = remove d1 (D.gen_key()) in
    assert(r1 = empty) ;
    assert(balanced r1) ;
    ()

  let test_remove_in_order () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list empty pairs1 in
    List.iter 
      (fun (k,v) -> 
        let r = remove d1 k in
        let _ = List.iter 
          (fun (k2,v2) ->
            if k = k2 then assert(lookup r k2 = None)
            else assert(lookup r k2 = Some v2)
          ) pairs1 in
        assert(balanced r)
      ) pairs1 ;
    ()

  let test_remove_reverse_order () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list_reversed empty pairs1 in
    List.iter 
      (fun (k,v) -> 
        let r = remove d1 k in
        let _ = List.iter 
          (fun (k2,v2) ->
            if k = k2 then assert(lookup r k2 = None)
            else assert(lookup r k2 = Some v2)
          ) pairs1 in
        assert(balanced r)
      ) pairs1 ;
    ()

  let test_remove_random_order () =
    let pairs5 = generate_random_list 100 in
    let d5 = insert_list empty pairs5 in
    let r5 = List.fold_right (fun (k,_) d -> remove d k) pairs5 d5 in
    List.iter (fun (k,_) -> assert(not (member r5 k))) pairs5 ;
    assert(r5 = empty) ;
    assert(balanced r5) ;
    () 

  let generate_pair_list_from_key (size: int) (k: key) : (key * value) list =
    let rec helper (size: int) (current: key) : (key * value) list =
      if size <= 0 then []
      else 
        let new_current = D.gen_key_gt current () in
        (new_current, D.gen_value()) :: (helper (size - 1) new_current)
    in
    helper size k

  let test_lookup2() =
    let el0 = D.gen_key () in
    let pairs = generate_pair_list_from_key 26 el0 in
    let rec nextkey k cnt = 
      if cnt >= 0 then (nextkey (D.gen_key_gt k ()) (cnt - 1)) 
      else k in
    let el26 = nextkey el0 26 in 
    let nextpairs = generate_pair_list_from_key 26 el26 in
    let d1 = insert_list empty pairs in
    List.iter (fun (k,v) -> assert ((lookup d1 k) = Some v)) pairs;
    List.iter (fun (k,v) -> assert ((lookup d1 k) = None)) nextpairs;
    List.iter (fun (k,v) -> assert ((lookup empty k) = None)) pairs;
    ()

  let test_member() =
    let el0 = D.gen_key () in
    let pairs = generate_pair_list_from_key 100 el0 in
    let rec nextkey k cnt = 
      if cnt >= 0 
      then (nextkey (D.gen_key_gt k ()) (cnt - 1)) 
      else k in
    let el100 = nextkey el0 100 in 
    let nextpairs = generate_pair_list_from_key 100 el100 in
    let d1 = insert_list empty pairs in
    List.iter (fun (k, v) -> assert (member d1 k)) pairs;
    List.iter (fun (k, v) -> assert (not (member d1 k))) nextpairs;
    List.iter (fun (k, v) -> assert (not (member empty k))) pairs;
    List.iter (fun (k, v) -> assert (not (member empty k))) nextpairs;
    ()

  let test_fold () =
    assert ((fold (fun k v a -> a + 1) 0 empty) = 0);
    let startel = D.gen_key () in
    let l1 = generate_pair_list_from_key 100 startel in 
    let d1 = insert_list empty l1 in
    (* Insert twice - should get equivalent dictionary *)
    let d2 = insert_list d1 l1 in
    let sum_dict = fold (fun k v a -> 
                        assert (List.exists (fun (n,u) -> n = k && u = v) l1);
                        a + 1
                        ) 0 d1 in
    assert (sum_dict = 100);
    let sum_dict2 = fold (fun k v a -> 
                         assert (List.exists (fun (n,u) -> n = k && u = v) l1);
                         a + 1
                         ) 0 d2 in
    assert (sum_dict2 = 100);
    ()

  let test_choose () =
    assert (choose empty = None);
    let startel = D.gen_key () in
    let l1 = generate_pair_list_from_key 100 startel in 
    let d1 = insert_list empty l1 in
    (* Insert twice - should get equivalent set *)
    let d2 = insert_list d1 l1 in
      let rec test_choose_helper st =
        match choose st with
          | None -> 0;
          | Some (k, v, newdict) -> 
            assert (List.exists (fun (lk,lv) -> lk = k && lv = v) l1); 
            1 + (test_choose_helper newdict) in
              assert (test_choose_helper d1 = 100);
              assert (test_choose_helper d2 = 100);
    ()

  let test_insert () =
    (* Example of a tree with 13 lucky nodes. *)
    let int_to_test = [27; 41; 63; 80; 61; 50; 19; 55; 11; 42; 53; 81; 54] in 
    (* Example of a pair list with 100 ordered elements from 1 to 100. *)
    let pairs_1_to_100 = generate_pair_list 100 in
    (* Filter the pair list with the key integers from the list int_to_test *)
    let pairs_filtered = 
      List.filter (fun (k,v) -> (List.exists 
                                  (fun x -> x = int_of_string(string_of_key k))
                                  int_to_test
                                )
                  ) pairs_1_to_100 in  
                    let pairs_filtered_ordered=List.map 
                      (fun x -> List.find (fun (k,v) -> 
                                          int_of_string(string_of_key k) = x
                                          ) pairs_filtered
                      ) int_to_test in

    (* Store the tree after insert the list of k,v *)
    let tree_with_list=insert_list Leaf pairs_filtered_ordered in

    (* Declaration of fixed pairs (Helper pairs used in the comparison) *)
    let p27 = List.nth pairs_filtered_ordered 0 in
    let p41 = List.nth pairs_filtered_ordered 1 in
    let p63 = List.nth pairs_filtered_ordered 2 in
    let p80 = List.nth pairs_filtered_ordered 3 in
    let p61 = List.nth pairs_filtered_ordered 4 in
    let p50 = List.nth pairs_filtered_ordered 5 in
    let p19 = List.nth pairs_filtered_ordered 6 in
    let p55 = List.nth pairs_filtered_ordered 7 in
    let p11 = List.nth pairs_filtered_ordered 8 in
    let p42 = List.nth pairs_filtered_ordered 9 in
    let p53 = List.nth pairs_filtered_ordered 10 in
    let p81 = List.nth pairs_filtered_ordered 11 in
    let p54 = List.nth pairs_filtered_ordered 12 in

    (* Fixed tree *)
    let tl = Two(Two(Leaf,p11,Leaf),p19,Two(Leaf,p27,Leaf)) in 
    let tm  = Two(Two(Leaf,p42,Leaf),p50,Three(Leaf,p53,Leaf,p54,Leaf)) in
    let tr = Two(Two(Leaf,p61,Leaf),p63,Three(Leaf,p80,Leaf,p81,Leaf)) in
    let tree_fixed = Three(tl,p41,tm,p55,tr) in

    (* Both trees should be the same *)
    assert(tree_fixed = tree_with_list);
    assert(balanced tree_with_list) ;

    (* Test the case for duplicate elements *)
    let tree_with_list = insert_list tree_fixed pairs_filtered_ordered in
    assert(tree_fixed = tree_with_list);
    assert(balanced tree_with_list) ;
    ()

  let run_tests () = 
    test_balance() ; 
    test_lookup_choose () ;
    test_lookup2 () ;
    test_member () ;
    test_insert ();
    test_choose ();
    test_fold () ;
    test_remove_nothing() ;
    test_remove_from_nothing() ;
    test_remove_in_order() ;
    test_remove_reverse_order() ;
    test_remove_random_order() ; 
    ()

end




(******************************************************************)
(* Run our tests.                                                 *)
(******************************************************************)

(* Create a dictionary mapping ints to strings using our 
 * AssocListDict functor and run the tests *)
module IntStringListDict = AssocListDict(IntStringDictArg) ;;
IntStringListDict.run_tests();;

(* Create a dictionary mapping ints to strings using our 
 * BTDict functor and run the tests.
 * 
 * Uncomment out the lines below when you are ready to test your
 * 2-3 tree implementation. *)

module IntStringBTDict = BTDict(IntStringDictArg) ;;
IntStringBTDict.run_tests();;




(******************************************************************)
(* Make: a functor that creates a DICT by calling our             *)
(* AssocListDict or BTDict functors                               *)
(******************************************************************)
module Make (D:DICT_ARG) : (DICT with type key = D.key
  with type value = D.value) = 
  (* Change this line to the BTDict implementation when you are
   * done implementing your 2-3 trees. *)
  (*AssocListDict(D)*)
  BTDict(D)

