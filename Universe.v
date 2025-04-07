    (* Declare two types 'a' and 'b', each in their respective universes 'A' and 'B' *)
    Universe A B.

    (* Declare 'a' of type Type A and 'b' of type Type B *)
    Variable a : Type@{A}.
    Variable b : Type@{B}.

    (* The function type 'a -> b' lives in a universe that is one level higher than 
   the maximum of the universes A and B *)
    Check (a -> b : Type@{1 + max(A, B)}).