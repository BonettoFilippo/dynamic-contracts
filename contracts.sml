(*what this module needs to do:
    - execute the program using the run function
    - see if any exception arise, in particular dynamic type errors
    - try to find out who is at fault? maybe use annotated tree in the process
    - start with first order functions but keep in mind the extension
    *)

structure contracts : COONTRACTS = struct

    fun execute (exp: expressions.exp) : eval.value =
        let 
            val result = eval.run exp handle
                DynamicTypeError _ => handle_dyn_type_error exp
              | e => raise e
        in
            result
        end

    fun handle_dyn_type_error (exp: expressions.exp) : eval.value =
        
        (* need to give valuable messages when throwing the exceptions. 
        need to be able to refer to the internal and outer types. 
        
        then i need to add an equality contraint on that.
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
            *)

end