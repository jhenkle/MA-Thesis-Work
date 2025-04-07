    (* First define the identity function of the type plant *)
    Definition id {plant : Type} (x : plant) := x.
    Definition identity_plant (x : plant) := x.

    (* s.t. *)
    identity_plant : plant -> plant

    (* Then show that bud, flower, and fruit are all members of type plant *)
    Inductive plant : Type :=
    | bud : plant
    | flower : plant
    | fruit : plant.

    (* Show the generic path p between the three elements *)
    Definition p : bud = flower -> flower = fruit -> bud = fruit :=
    fun Hbudflower Hflowerfruit => eq_trans Hbudflower Hflowerfruit.

    (* Following the operation for assigning function types,
   it follows that the mapping (bud to flower to fruit) should have the type plant -> plant *)

    Definition plant_mapper (x : plant) : plant :=
    match x with
    | bud => flower
    | flower => fruit
    | fruit => suc(fruit)
    end.
    (* Here suc, is a function which returns the next (successor) element in the type plant *)
    
    (* The identity map is of type plant -> plant *)
    Check identity_plant.
    (* identity_plant : plant -> plant *)

    (* The mapping plant_mapper is also of type plant -> plant, demonstrating that functions 
   on the same type are at the same type level. *)
    Check plant_mapper.
    (* plant_mapper : plant -> plant *)