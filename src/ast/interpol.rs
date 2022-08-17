#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum InterpolPart<T> {
    Literal(T),
    Interpolation(super::Interpol),
}
