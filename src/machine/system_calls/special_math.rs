use crate::machine::Number;
use crate::Machine;
//use crate::puruspe;
use puruspe::error::*;

impl Machine {

    #[inline(always)]
    pub(crate) fn erf(&mut self) {
        let x = self.deref_register(1);
        let x = match Number::try_from(x) {
            Ok(Number::Float(n)) => n.into_inner(),
            Ok(Number::Fixnum(n)) => n.get_num() as f64,
            Ok(Number::Integer(n)) => n.to_f64().value(),
            _ => {
                unreachable!()
            }
        };
        let erf_x = float_alloc!(erf(x), self.machine_st.arena);
        let return_value = self.deref_register(2);
        self.machine_st.unify_f64(erf_x, return_value);
    }

    #[inline(always)]
    pub(crate) fn erfc(&mut self) {
        let x = self.deref_register(1);
        let x = match Number::try_from(x) {
            Ok(Number::Float(n)) => n.into_inner(),
            Ok(Number::Fixnum(n)) => n.get_num() as f64,
            Ok(Number::Integer(n)) => n.to_f64().value(),
            _ => {
                unreachable!()
            }
        };
        let erfc_x = float_alloc!(erfc(x), self.machine_st.arena);
        let return_value = self.deref_register(2);
        self.machine_st.unify_f64(erfc_x, return_value);
    }

}
