(* Definitions for sets. *)

exception TODO

(* An interface for set modules *)
module type SET = 
sig
  type elt  (* type of elements in the set *)
  type set  (* abstract type for the set *)

  val empty : set

  val is_empty : set -> bool

  val insert : elt -> set -> set

  (* same as insert x empty *)
  val singleton : elt -> set

  val union : set -> set -> set
  val intersect : set -> set -> set

  (* remove an element from the set -- if the
   * element isn't present, does nothing. *)
  val remove : elt -> set -> set

  (* returns true iff the element is in the set *)
  val member : set -> elt -> bool

  (* chooses some member from the set, removes it 
   * and returns that element plus the new set.  
   * If the set is empty, returns None. *)
  val choose : set -> (elt * set) option

  (* fold a function across the elements of the set
   * in some unspecified order. *)
  val fold : (elt -> 'a -> 'a) -> 'a -> set -> 'a

  (* functions to convert our types to a string. useful for debugging. *)
  val string_of_set : set -> string
  val string_of_elt : elt -> string

  (* runs our tests. See TESTING EXPLANATION *)
  val run_tests : unit -> unit
end



(* parameter to Set modules -- we must pass in some 
 * type for the elements of a set, a comparison
 * function, and a way to stringify it.
 *)
module type COMPARABLE = 
sig
  type t
  val compare : t -> t -> Order.order
  val string_of_t : t -> string

  (* The functions below are used for testing. See TESTING
   * EXPLANATION *)

  (* Generate a value of type t. The same t is always returned *)
  val gen : unit -> t

  (* Generate a random value of type t. *)
  val gen_random : unit -> t

  (* Generate a t greater than the argument. *)
  val gen_gt : t -> unit -> t

  (* Generate a t less than the argument. *)
  val gen_lt : t -> unit -> t

  (* Generate a t between the two arguments. Return None if no such
   * t exists. *)
  val gen_between : t -> t -> unit -> t option
end



(* An example implementation of our COMPARABLE signature. Use this
 * struct for testing. *)
module IntComparable : COMPARABLE =
struct
  open Order
  type t = int
  let compare x y = if x < y then Less else if x > y then Greater else Eq
  let string_of_t = string_of_int
  let gen () = 0
  let gen_random =
    let _ = Random.self_init () in
    (fun () -> Random.int 10000)
  let gen_gt x () = x + 1
  let gen_lt x () = x - 1
  let gen_between x y () = 
    let (lower, higher) = (min x y, max x y) in
    if higher - lower < 2 then None else Some (higher - 1)
end



(* A simple, list-based implementation of sets. *)
module ListSet(C: COMPARABLE) : (SET with type elt = C.t) = 
struct
  open Order
  type elt = C.t 
  type set = elt list

  (* INVARIANT: sorted, no duplicates *)
  let empty = []
  let is_empty xs = 
    match xs with 
      | [] -> true
      | _ -> false
  let singleton x = [x]
  let rec insert x xs = 
    match xs with 
      | [] -> [x]
      | y::ys -> 
        (match C.compare x y with 
          | Greater -> y::(insert x ys)
          | Eq -> xs
          | Less -> x::xs
        )

  let union xs ys = List.fold_right insert xs ys
  let rec remove y xs = 
    match xs with 
      | [] -> []
      | x::xs1 -> 
        (match C.compare y x with 
          | Eq -> xs1
          | Less -> xs
          | Greater -> x::(remove y xs1))

  let rec intersect xs ys = 
    match xs, ys with 
      | [], _ -> []
      | _, [] -> []
      | xh::xt, yh::yt -> 
        (match C.compare xh yh with 
          | Eq -> xh::(intersect xt yt)
          | Less -> intersect xt ys
          | Greater -> intersect xs yt)

  let rec member xs x = 
    match xs with 
      | [] -> false
      | y::ys -> 
        (match C.compare x y with
          | Eq -> true
          | Greater -> member ys x
          | Less -> false
        )

  let choose xs = 
    match xs with 
      | [] -> None
      | x::rest -> Some (x,rest)
  let fold f e = List.fold_left (fun a x -> f x a) e 
    
  let string_of_elt = C.string_of_t
  let string_of_set (s: set) : string = 
    let f = (fun y e -> y ^ "; " ^ C.string_of_t e) in
    "set([" ^ (List.fold_left f "" s) ^ "])"


  (****************************************************************)
  (* Tests for our ListSet functor                                *)
  (* These are just examples of tests, your tests should be a lot *)
  (* more thorough than these.                                    *)
  (****************************************************************)

  (* adds a list of (key,value) pairs in left-to-right order *)
  let insert_list (d: set) (lst: elt list) : set = 
    List.fold_left (fun r k -> insert k r) d lst

  let rec generate_random_list (size: int) : elt list =
    if size <= 0 then []
    else (C.gen_random()) :: (generate_random_list (size - 1))

  let test_insert () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    List.iter (fun k -> assert(member s1 k)) elts ;
    ()

  let test_remove () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    let s2 = List.fold_right (fun k r -> remove k r) elts s1 in
    List.iter (fun k -> assert(not (member s2 k))) elts ;
    ()

  let test_union () =
    ()

  let test_intersect () =
    ()

  let test_member () =
    ()

  let test_choose () =
    ()

  let test_fold () =
    ()

  let test_is_empty () =
    ()

  let test_singleton () =
    ()

  let run_tests () = 
    test_insert () ;
    test_remove () ;
    test_union () ;
    test_intersect () ;
    test_member () ;
    test_choose () ;
    test_fold () ;
    test_is_empty () ;
    test_singleton () ;
    ()

end


(******************************************************************)
(* DictSet: a functor that creates a SET by calling our           *)
(* Dict.Make functor                                              *)
(******************************************************************)

module DictSet(C : COMPARABLE) : (SET with type elt = C.t) = 
struct
  module D = Dict.Make(struct
    type key = C.t
    type value = unit
    let compare x y = C.compare x y
    let string_of_key = C.string_of_t
    let string_of_value v = "-"
    let gen_key () = C.gen ()
    let gen_key_gt x () = C.gen_gt x ()
    let gen_key_lt x () = C.gen_lt x ()
    let gen_key_between x y () = C.gen_between x y ()
    let gen_key_random () = C.gen_random ()
    let gen_value () = ()
    let gen_pair () = (gen_key (), gen_value ())
  end)

  type elt = D.key
  type set = D.dict
  let empty = D.empty

  (* implement the rest of the functions in the signature! *)

  let string_of_elt = D.string_of_key
  let string_of_set s = D.string_of_dict s

  let is_empty set = set = D.empty

  let insert elt set = D.insert set elt ()

  (* same as insert x empty *)
  let singleton elt = D.insert empty elt ()

  let union s1 s2 = D.fold (fun k v a -> D.insert a k v) s2 s1
  let intersect s1 s2 = D.fold (fun k v a -> 
    if D.member s2 k 
    then D.insert a k v 
    else a) empty s1

  (* remove an element from the set -- if the
   * element isn't present, does nothing. *)
  let remove elt s = D.remove s elt

  (* returns true iff the element is in the set *)
  let member s e = D.member s e

  (* chooses some member from the set, removes it 
   * and returns that element plus the new set.  
   * If the set is empty, returns None. *)
  let choose s = match D.choose s with 
    | None -> None
    | Some (k,v,d) -> Some(k,d)

  (* fold a function across the elements of the set
   * in some unspecified order. *)
  let fold f a s = D.fold (fun k v a -> f k a) a s

  (****************************************************************)
  (* Tests for our DictSet functor                                *)
  (* Use the tests from the ListSet functor to see how you should *)
  (* write tests. However, you must write a lot more              *)
  (* comprehensive tests to test ALL your functions.              *)
  (****************************************************************)

  (* add your test functions to run_tests *)
  let insert_list (d: set) (lst: elt list) : set = 
    List.fold_left (fun r k -> insert k r) d lst

  let rec generate_random_list (size: int) : elt list =
    if size <= 0 then []
    else (C.gen_random()) :: (generate_random_list (size - 1))

  let rec gen_list count el = 
    if count <= 0 then [] else el :: gen_list (count - 1) (C.gen_gt el ())

  let test_insert () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    List.iter (fun k -> assert(member s1 k)) elts ;
    (* Test for duplicate insertions *)
    let s2 = insert_list s1 elts in
    List.iter (fun k -> assert(member s2 k)) elts ;
    assert ((fold (fun x a -> a + 1) 0 s2) = (fold (fun x a -> a + 1) 0 s1));
    assert (is_empty (List.fold_right (fun x a -> remove x a) elts s2));
    let startel = C.gen () in
    let seqlist = gen_list 200 startel in
    let s3 = insert_list (insert_list empty seqlist) seqlist in
    assert ((fold (fun x a -> a + 1) 0 s3) = 200);
    assert (is_empty (List.fold_right (fun x a -> remove x a) seqlist s3));
    ()


  let test_remove () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    let s2 = List.fold_right (fun k r -> remove k r) elts s1 in
    List.iter (fun k -> assert(not (member s2 k))) elts ;
    assert (is_empty s2);
    (* Test removing non-existent elements *)
    let el0 = C.gen () in
    let el1 = C.gen_gt el0 () in
    let seqlist = gen_list 200 el1 in
    let s3 = insert_list empty seqlist in
    assert ((remove el0 s3) = s3);
    ()

  let test_union () =
    assert ((union empty empty) = empty);
    assert (is_empty (union empty empty));
    let l1 = generate_random_list 100 in 
    let l2 = generate_random_list 50 in 
    let l3 = generate_random_list 70 in 
    let s1 = insert_list empty (l1) in
    let s2 = insert_list empty (l2) in
    let union_empty_s1 = union empty s1 in
    List.iter (fun x -> assert (member union_empty_s1 x)) l1;
    assert (is_empty (
     List.fold_right 
    (fun x a -> remove x a) l1 union_empty_s1));
    let union_empty_s2 = union empty s2 in
    List.iter (fun x -> assert (member union_empty_s2 x)) l2;
    assert (is_empty (
      List.fold_right 
    (fun x a -> remove x a) l2 union_empty_s2));
    let union_s1_empty = union s1 empty in
    List.iter (fun x -> assert (member union_s1_empty x)) l1 ;
    assert (is_empty (
      List.fold_right 
    (fun x a -> remove x a) l1 union_s1_empty));
    let union_s2_empty = union s2 empty in
    List.iter (fun x -> assert (member union_s2_empty x)) l2;
    assert (is_empty (
      List.fold_right 
    (fun x a -> remove x a) l2 union_s2_empty));
    let union_s1_s2 = union s1 s2 in
    List.iter (fun x -> assert (member union_s1_s2 x)) l1;
    List.iter (fun x -> assert (member union_s1_s2 x)) l2;
    List.iter (fun x -> assert (member s1 x || member s2 x || 
                  (not (member union_s1_s2 x)))) l3 ;
    let union_s1_s2 = List.fold_right 
      (fun x a -> remove x a) l1 union_s1_s2 in 
    let union_s1_s2 = List.fold_right 
      (fun x a -> remove x a) l2 union_s1_s2 in 
    assert (is_empty union_s1_s2);
    ()

  let test_intersect () =
    assert ((intersect empty empty) = empty);
    assert (is_empty (intersect empty empty));
    let l1 = generate_random_list 100 in 
    let l2 = generate_random_list 50 in 
    let l3 = generate_random_list 70 in 
    (* Make sure s1 doesn't contain anything from l2 *)
    let s1 = insert_list empty (l1) in
    let s1 = List.fold_right (fun x s -> remove x s) l2 s1 in
    let s1 = insert_list s1 l3 in
    (* Make sure s2 doesn't contain anything from l1 *)
    let s2 = insert_list empty (l2) in
    let s2 = List.fold_right (fun x s -> remove x s) l1 s2 in
    let s2 = insert_list s2 l3 in
    let s3 = insert_list empty (l3) in
    (* l3/s3 elements - intersection *)
    let intersect_s1_s2 = intersect s1 s2 in
    List.fold_right (fun x a -> assert (member intersect_s1_s2 x)) l3 ();
    List.fold_right (fun x a -> assert (
      (List.exists (fun el -> el = x) l3) || 
    not (member intersect_s1_s2 x))) l1 ();
    List.fold_right (fun x a -> assert (
      (List.exists (fun el -> el = x) l3) || 
    not (member intersect_s1_s2 x))) l2 ();
    let intersect_s1_s2 = List.fold_right (fun x s -> 
      remove x s) l3 intersect_s1_s2 in
    assert (is_empty intersect_s1_s2);
    assert (is_empty (intersect empty s1));
    assert (is_empty (intersect s1 empty));
    assert (is_empty (intersect empty s2));
    assert (is_empty (intersect s2 empty));
    assert (is_empty (intersect empty s3));
    assert (is_empty (intersect s3 empty));
    ()

  let test_member () =
    let l1 = generate_random_list 100 in 
    let empty_set = empty in
    List.fold_right (fun x a -> assert (not (member empty_set x))) l1 ();
    let s1 = insert_list empty (l1) in
    List.fold_right (fun x a -> assert (member s1 x)) l1 ();
    ()

  let test_choose () =
    assert (choose empty = None);
    let startel = C.gen () in
    let l1 = gen_list 100 startel in 
    let s1 = insert_list empty l1 in
    (* Insert twice - should get equivalent set *)
    let s2 = insert_list s1 l1 in
    let rec test_choose_helper st =
      match choose st with
        | None -> 0;
        | Some (el, newset) -> 
          assert (List.exists (fun x -> x = el) l1); 
          1 + (test_choose_helper newset) in
            assert (test_choose_helper s1 = 100);
            assert (test_choose_helper s2 = 100);
    ()

  let test_fold () =
    assert ((fold (fun k a -> a + 1) 0 empty) = 0);
    let startel = C.gen () in
    let l1 = gen_list 100 startel in 
    let s1 = insert_list empty l1 in
    (* Insert twice - should get equivalent set *)
    let s2 = insert_list s1 l1 in
    let sum_set = fold (fun x a -> 
      assert (List.exists (fun el -> el = x) l1); a + 1) 0 s1 in
    assert (sum_set = 100);
    let sum_set2 = fold (fun x a -> 
      assert (List.exists (fun el -> el = x) l1); a + 1) 0 s2 in
    assert (sum_set2 = 100);
    ()

  let test_is_empty () =
    assert (is_empty empty);
    assert (not (is_empty (insert (C.gen_random ()) empty)));
    assert (not (is_empty (insert_list empty (generate_random_list 100))));
    ()

  let test_singleton () =
    let el1 = C.gen () in
    let s = singleton el1 in
    assert (not (is_empty s));
    assert (member s el1);
    assert (is_empty (remove el1 s));
    ()

  let run_tests () = 
    test_insert () ;
    test_remove () ;
    test_union () ;
    test_intersect () ;
    test_member () ;
    test_choose () ;
    test_fold () ;
    test_is_empty () ;
    test_singleton () ;
    ()

end




(******************************************************************)
(* Run our tests.                                                 *)
(******************************************************************)

(* Create a set of ints using our ListSet functor. *)
module IntListSet = ListSet(IntComparable) ;;
IntListSet.run_tests();;

(* Create a set of ints using our DictSet functor
 * 
 * Uncomment out the lines below when you are ready to test your
 * 2-3 dict set implementation *)


module IntDictSet = DictSet(IntComparable) ;;
IntDictSet.run_tests();;



(******************************************************************)
(* Make: a functor that creates a SET by calling our              *)
(* ListSet or DictSet functors                                    *)
(******************************************************************)
module Make(C : COMPARABLE) : (SET with type elt = C.t) = 
  (* Change this line to use our dictionary implementation when your are 
   * finished. *)
(*  ListSet (C)*)
  DictSet (C)

