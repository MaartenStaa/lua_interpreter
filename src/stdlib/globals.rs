use crate::value::LuaValue;

pub(crate) fn print(input: Vec<LuaValue>) -> LuaValue {
    for (i, value) in input.into_iter().enumerate() {
        if i != 0 {
            print!("\t");
        }
        print!("{}", value);
    }
    println!();

    LuaValue::Nil
}
