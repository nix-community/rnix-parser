#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum InterpolPart {
    Literal(String),
    Interpolation(super::Interpol),
}
