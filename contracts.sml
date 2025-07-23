(*what this module needs to do:
    - execute the program using the run function
    - see if any exception arise, in particular dynamic type errors
    - try to find out who is at fault? maybe use annotated tree in the process
    - start with first order functions but keep in mind the extension
    *)

structure contracts : COONTRACTS = struct

    fun execute (exp: expressions.exp) : evalannotated.ann_value =
        let 
            val (annotated_exp, constraints) = constraintsyntax.generate exp
            val result = evalannotated.run_ann annotated_exp handle
                DynamicTypeError _ => handle_dyn_type_error annotated_exp constraints
              | e => raise e
        in
            result
        end

    fun handle_dyn_type_error (exp: constraintsyntax.ann_exp, con: constraints.constraint list) : evalannotated.ann_value =

        (* need to give valuable messages when throwing the exceptions. 
        need to be able to refer to the internal and outer types. 
        
        maybe: 
            - change the infer function so that id doesn't generate errors
            - should add something new to the annotated expression type
                - these should be an error parameter, to keep track of who generated the error. 
                - i think that once i find an error i should be able to stop the annotation
                  as i don't need anything beyond that point. 
        
        in this function i need to be able to handle both the worklist (the set
        of constraints) and the annotated tree. 

        then i need to add an equality constraint on that.
        fir first order functions i can just use the already 
        existing contraints and check if there are any dynamic values when generating
        
        i think i also need a new funciton to change the value of a uref. 
        when i unify i most likely go to dynamic, but in this situation i dont want to change but enforce
        i can then check if the value before and after enforcing it to be equal to something are changed
        
        then i can do as i mentioned to fritz: 
            - for simple hardcoded functions like negation or plus1 the problem is always within the caller. 
            - for actual functions if the result value is changed there is a problem within the function
              otherwise it is within the caller.
        i need to keep in mind that if the problem is within the caller, i need to find who generated the value

        VERY IMPORTANT   
        one solution is to add to the uref the line that generated that type
        this means that i need some value line for newly generated types (something like -1?)
        i think that only EInt, EBool, and ELam should be able to generate new values for this field. 
            
            *)

        
end