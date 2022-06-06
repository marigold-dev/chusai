#include "../../commons/refutation_interface.mligo"

let make_segment (start:hash) (end_:hash) (size:size) =
    (start,end_,size)

let size (s : segment ) =
    s.2

let start (s : segment ) =
    s.0

let end_ (s : segment ) =
    s.1

let choose (choice, split : choice * split)  = 
    match choice with
    | Left  -> split.0
    | Right -> split.1

let check_split_against_segment ((s1,s2),segment : split * segment) : bool = 
       (start segment) = (start s1)
    && (end_ s1) = (start s2)
    && (size segment) = ((size s1) + (size s2))
    && (end_ segment) <> (end_ s2)    