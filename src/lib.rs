/// Percentage type that tracks whether the intenal value represents percentage
/// points or a fractional part.
pub enum Percentage<T> {
    /// Internal value is fractional
    Fraction(Fraction<T>),
    /// Internal value is points
    Points(Points<T>),
}

/// Wrapper type that contains a fractional value
pub struct Fraction<T>(T);

/// Wrapper type that contains a percentage points value
pub struct Points<T>(T);

