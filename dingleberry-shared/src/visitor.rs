use crate::error::SpruceErr;

pub trait Visitor<T, U> {
    fn visit(&mut self, item: &T) -> Result<U, SpruceErr>;

    fn visit_identifier(&mut self, item: &T) -> Result<U, SpruceErr>;
    fn visit_literal(&mut self, item: &T) -> Result<U, SpruceErr>;
    fn visit_struct_literal(&mut self, item: &T) -> Result<U, SpruceErr>;
    fn visit_tuple_literal(&mut self, item: &T) -> Result<U, SpruceErr>;
    fn visit_array_literal(&mut self, item: &T) -> Result<U, SpruceErr>;
    fn visit_expression_statement(&mut self, item: &T) -> Result<U, SpruceErr>;

    fn visit_binary_op(&mut self, item: &T) -> Result<U, SpruceErr>;
    fn visit_unary_op(&mut self, item: &T) -> Result<U, SpruceErr>;
    fn visit_logical_op(&mut self, item: &T) -> Result<U, SpruceErr>;

    fn visit_parameter(&mut self, item: &T) -> Result<U, SpruceErr>;
    fn visit_function(&mut self, item: &T) -> Result<U, SpruceErr>;
    fn visit_function_call(&mut self, item: &T) -> Result<U, SpruceErr>;

    fn visit_var_declaration(&mut self, item: &T) -> Result<U, SpruceErr>;
    fn visit_var_declarations(&mut self, item: &T) -> Result<U, SpruceErr>;
    fn visit_var_assign(&mut self, item: &T) -> Result<U, SpruceErr>;
    fn visit_var_assign_equal(&mut self, item: &T) -> Result<U, SpruceErr>;

    fn visit_if_statement(&mut self, item: &T) -> Result<U, SpruceErr>;
    fn visit_for_statement(&mut self, item: &T) -> Result<U, SpruceErr>;

    fn visit_index_getter(&mut self, item: &T) -> Result<U, SpruceErr>;
    fn visit_index_setter(&mut self, item: &T) -> Result<U, SpruceErr>;
    fn visit_property_getter(&mut self, item: &T) -> Result<U, SpruceErr>;
    fn visit_property_setter(&mut self, item: &T) -> Result<U, SpruceErr>;

    fn visit_switch_statement(&mut self, item: &T) -> Result<U, SpruceErr>;
    fn visit_switch_case(&mut self, item: &T) -> Result<U, SpruceErr>;

    fn visit_this(&mut self, item: &T) -> Result<U, SpruceErr>;
    fn visit_return_statement(&mut self, item: &T) -> Result<U, SpruceErr>;
    fn visit_body(&mut self, item: &T, new_scope: bool) -> Result<U, SpruceErr>;
    fn visit_include(&mut self, item: &T) -> Result<U, SpruceErr>;
    fn visit_module(&mut self, item: &T) -> Result<U, SpruceErr>;
    fn visit_program(&mut self, item: &T) -> Result<U, SpruceErr>;
    fn visit_empty(&mut self, item: &T) -> Result<U, SpruceErr>;
}
