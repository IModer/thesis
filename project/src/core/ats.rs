
/*

    Types: Int, Bool

    A Term is something that can have a value, so something that can be evaluated
    
    +, *, 

*/

use std::{ops::Range, collections::HashMap};

// TODO
pub enum Type {
    Int, Bool
}

/*

for:    for i in a..b    // i,a,b elem ZZ
        {
            Term_1;
            Term_2;
            ...
            Term_n;
        }
while:  while b    // b elem LL
        {
            Term_1;
            Term_2;
            ...
            Term_n;
        }
if :    if b       // b elem LL
        {
            Term_1;
            Term_2;
            ...
            Term_n;
        }
        // optional
        <else {  
            Term_1;
            Term_2;
            ...
            Term_k;
        }>+
        
*/

type Name = String; //TODO: deBrujin indicies
type IntType = i16; 

pub enum Term {
    For {
        var : Name,                     // do we need to store it ? if yes then what? its name?
        range : std::ops::Range<IntType>,   // change to some IntType that can be chosen before running like: --inttype i32
        body : Vec<Box<Term>>,
    },
    While {
        cond : Box<Term>,
        body : Vec<Box<Term>>,
    },
    IfElse {
        cond : Box<Term>,       // invariant: cond has type bool
        if_body : Vec<Box<Term>>,
        else_body : Option<Vec<Box<Term>>>,  // or we can leave if as Vec<B<Term>> and if len == 0 then there is no else part
    },
    Print(Box<Term>),
    Int,
}

pub enum Value {
    Int(IntType)
}

impl Term {
    // Iterative preorder
    fn eval_term(t: Term) {
        
        // we need a context, foirst name - value
        // value is has Rust type

        let mut context : HashMap<&Name, Value> = HashMap::new();

        let mut stack : Vec<&Term> = Vec::new();
        stack.push(&t);


        while stack.len() > 0
        {
            let c = stack.pop().expect("stack is not empty");

            //Process c
            match c {
                Term::For { var, range, body } => 
                {
                    // we put var into the context
                    // it will have value : range.start, range.start+1, ..., range.end
                    // we eval everything in body with the fact that var = range.start, encoded in context
                    for i in range.clone()
                    {
                        context.insert(&var, Value::Int(i));
                        
                    }
                },
                Term::While { cond, body } => todo!(),
                Term::IfElse { cond, if_body, else_body } => todo!(),
                Term::Print(_) => todo!(),
                Term::Int => todo!(),
            }
        }
    }
}
