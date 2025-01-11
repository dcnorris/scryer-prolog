use crate::machine::Number;
use crate::Machine;
use puruspe::error::*;

macro_rules! number_as_f64 {
    ($x: expr) => {{
        match Number::try_from($x) {
            Ok(Number::Float(n)) => n.into_inner(),
            Ok(Number::Fixnum(n)) => n.get_num() as f64,
            Ok(Number::Integer(n)) => n.to_f64().value(),
            _ => {
                unreachable!()
            }
        }
    }};
}

macro_rules! return_f64_reg {
    ($self: ident, $val: ident, $reg: literal) => {{
        let return_value = $self.deref_register($reg);
        $self.machine_st.unify_f64($val, return_value);
    }};
}

impl Machine {

    #[inline(always)]
    pub(crate) fn erf(&mut self) {
        let x = number_as_f64!(self.deref_register(1));
        let erf_x = float_alloc!(erf(x), self.machine_st.arena);
        return_f64_reg!(self, erf_x, 2);
    }

    #[inline(always)]
    pub(crate) fn erfc(&mut self) {
        let x = number_as_f64!(self.deref_register(1));
        let erfc_x = float_alloc!(erfc(x), self.machine_st.arena);
        return_f64_reg!(self, erfc_x, 2);
    }

    #[inline(always)]
    pub(crate) fn inverf(&mut self) {
        let erf_x = number_as_f64!(self.deref_register(1));
        let x = float_alloc!(inverf(erf_x), self.machine_st.arena);
        return_f64_reg!(self, x, 2);
    }

    #[inline(always)]
    pub(crate) fn inverfc(&mut self) {
        let erfc_x = number_as_f64!(self.deref_register(1));
        let x = float_alloc!(inverfc(erfc_x), self.machine_st.arena);
        return_f64_reg!(self, x, 2);
    }

}
