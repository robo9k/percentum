use std::ops::{Add, Div, Mul, Sub};

/// A number that can be used as a [`Percentage`] value
pub trait Number:
    Sized
    + Copy
    + PartialEq
    + PartialOrd
    + Add<Output = Self>
    + Div<Output = Self>
    + Mul<Output = Self>
    + Sub<Output = Self>
{
    /// Does the value equate to zero?
    fn is_zero(&self) -> bool;

    /// The percentage scale factor that converts between fraction and points
    fn one_hundred() -> Self;
}

/// Percentage type that tracks whether the intenal value represents percentage
/// points or a fractional part.
#[derive(Clone, Copy)]
pub enum Percentage<T> {
    /// Internal value is fractional
    Fraction(Fraction<T>),
    /// Internal value is points
    Points(Points<T>),
}

/// Wrapper type that contains a fractional value
#[derive(Clone, Copy, PartialEq, PartialOrd)]
pub struct Fraction<T>(T);

/// Wrapper type that contains a percentage points value
#[derive(Clone, Copy, PartialEq, PartialOrd)]
pub struct Points<T>(T);

impl<T> Percentage<T>
where
    T: Number,
{
    /// Construct a [`Percentage`] from a fractional value
    pub const fn from_fraction(fraction: T) -> Self {
        Self::Fraction(Fraction(fraction))
    }

    /// Construct a [`Percentage`] from a percentage points value
    pub const fn from_points(points: T) -> Self {
        Self::Points(Points(points))
    }

    /// Obtain the fractional representation of the [`Percentage`]
    pub fn to_fraction(self) -> Fraction<T> {
        match self {
            Self::Fraction(fraction) => fraction,
            Self::Points(points) => Fraction::from(points),
        }
    }

    /// Obtain the points representation of the [`Percentage`]
    pub fn to_points(self) -> Points<T> {
        match self {
            Self::Fraction(fraction) => Points::from(fraction),
            Self::Points(points) => points,
        }
    }

    /// Compute the quantity that represents the percentage of `other`
    pub fn apply_to(self, other: T) -> T {
        self.to_fraction().into_inner() * other
    }

    /// Compute the percentage gained or lost between the initial and final
    /// value.
    pub fn gain_loss(final_value: T, initial_value: T) -> Option<Self> {
        (!initial_value.is_zero())
            .then(|| Percentage::from_fraction((final_value - initial_value) / initial_value))
    }

    /// Compute the percentage of the `whole` represented by the `part`
    pub fn part_of_the_whole(part: T, whole: T) -> Option<Self> {
        (!whole.is_zero()).then(|| Self::Fraction(Fraction(part / whole)))
    }
}

impl<T> PartialEq for Percentage<T>
where
    T: Number,
{
    fn eq(&self, other: &Self) -> bool {
        self.to_points() == other.to_points()
    }
}

impl<T> PartialOrd for Percentage<T>
where
    T: Number,
{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.to_points().partial_cmp(&other.to_points())
    }
}

impl<T> From<Fraction<T>> for Points<T>
where
    T: Number,
{
    fn from(fraction: Fraction<T>) -> Self {
        Points(fraction.into_inner() * T::one_hundred())
    }
}

impl<T> From<Points<T>> for Fraction<T>
where
    T: Number,
{
    fn from(points: Points<T>) -> Self {
        Fraction(points.into_inner() / T::one_hundred())
    }
}

impl<T> Fraction<T> {
    pub fn into_inner(self) -> T {
        self.0
    }
}

impl<T> Points<T> {
    pub fn into_inner(self) -> T {
        self.0
    }
}

impl Number for f32 {
    fn is_zero(&self) -> bool {
        *self == 0.0
    }

    fn one_hundred() -> Self {
        100.0
    }
}

impl Number for f64 {
    fn is_zero(&self) -> bool {
        *self == 0.0
    }

    fn one_hundred() -> Self {
        100.0
    }
}

#[cfg(feature = "decimal")]
impl Number for rust_decimal::Decimal {
    fn is_zero(&self) -> bool {
        rust_decimal::Decimal::is_zero(&self)
    }

    fn one_hundred() -> Self {
        rust_decimal::Decimal::ONE_HUNDRED
    }
}

impl<T> Add for Fraction<T>
where
    T: Number,
{
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

impl<T> Sub for Fraction<T>
where
    T: Number,
{
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self(self.0 - rhs.0)
    }
}

impl<T> Add for Points<T>
where
    T: Number,
{
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

impl<T> Sub for Points<T>
where
    T: Number,
{
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self(self.0 - rhs.0)
    }
}

impl<T> Add for Percentage<T>
where
    T: Number,
{
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Fraction(lhs), Self::Fraction(rhs)) => Self::Fraction(lhs + rhs),
            (Self::Fraction(fraction), Self::Points(points)) => {
                Self::Fraction(fraction + Fraction::from(points))
            }
            (Self::Points(points), Self::Fraction(fraction)) => {
                Self::Points(points + Points::from(fraction))
            }
            (Self::Points(lhs), Self::Points(rhs)) => Self::Points(lhs + rhs),
        }
    }
}

impl<T> Sub for Percentage<T>
where
    T: Number,
{
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Fraction(lhs), Self::Fraction(rhs)) => Self::Fraction(lhs - rhs),
            (Self::Fraction(fraction), Self::Points(points)) => {
                Self::Fraction(fraction - Fraction::from(points))
            }
            (Self::Points(points), Self::Fraction(fraction)) => {
                Self::Points(points - Points::from(fraction))
            }
            (Self::Points(lhs), Self::Points(rhs)) => Self::Points(lhs - rhs),
        }
    }
}

impl Add<Percentage<f32>> for f32 {
    type Output = f32;

    fn add(self, rhs: Percentage<f32>) -> Self::Output {
        self + rhs.apply_to(self)
    }
}

impl Sub<Percentage<f32>> for f32 {
    type Output = f32;

    fn sub(self, rhs: Percentage<f32>) -> Self::Output {
        self - rhs.apply_to(self)
    }
}

impl<T> std::fmt::Debug for Percentage<T>
where
    T: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Fraction(Fraction(fraction)) => write!(f, "Fraction({:?})", fraction),
            Self::Points(Points(points)) => write!(f, "Points({:?})", points),
        }
    }
}

impl<T> std::fmt::Display for Percentage<T>
where
    T: Number + std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.to_points().into_inner().fmt(f)?;
        f.write_str("%")
    }
}

#[cfg(feature = "serde")]
pub mod serde_points {
    use super::{Number, Percentage};
    use serde::{Deserialize, Deserializer, Serialize, Serializer};

    pub fn deserialize<'de, D, T>(deserializer: D) -> Result<Percentage<T>, D::Error>
    where
        D: Deserializer<'de>,
        T: Deserialize<'de> + Number,
    {
        let points = T::deserialize(deserializer)?;

        Ok(Percentage::from_points(points))
    }

    pub fn serialize<S, T>(percentage: &Percentage<T>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
        T: Serialize + Number,
    {
        percentage.to_points().into_inner().serialize(serializer)
    }
}

#[cfg(feature = "serde")]
pub mod serde_points_maybe {
    use super::{Number, Percentage};
    use serde::{Deserialize, Deserializer, Serialize, Serializer};

    pub fn deserialize<'de, D, T>(deserializer: D) -> Result<Option<Percentage<T>>, D::Error>
    where
        D: Deserializer<'de>,
        T: Deserialize<'de> + Number,
    {
        let points = Option::<T>::deserialize(deserializer)?;

        Ok(points.map(|points| Percentage::from_points(points)))
    }

    pub fn serialize<S, T>(
        percentage: &Option<Percentage<T>>,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
        T: Serialize + Number,
    {
        percentage
            .map(|percentage| percentage.to_points().into_inner())
            .serialize(serializer)
    }
}

#[cfg(feature = "serde")]
pub mod serde_fraction {
    use super::{Number, Percentage};
    use serde::{Deserialize, Deserializer, Serialize, Serializer};

    pub fn deserialize<'de, D, T>(deserializer: D) -> Result<Percentage<T>, D::Error>
    where
        D: Deserializer<'de>,
        T: Deserialize<'de> + Number,
    {
        let fraction = T::deserialize(deserializer)?;

        Ok(Percentage::from_fraction(fraction))
    }

    pub fn serialize<S, T>(percentage: &Percentage<T>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
        T: Serialize + Number,
    {
        percentage.to_fraction().into_inner().serialize(serializer)
    }
}

#[cfg(feature = "serde")]
pub mod serde_fraction_maybe {
    use super::{Number, Percentage};
    use serde::{Deserialize, Deserializer, Serialize, Serializer};

    pub fn deserialize<'de, D, T>(deserializer: D) -> Result<Option<Percentage<T>>, D::Error>
    where
        D: Deserializer<'de>,
        T: Deserialize<'de> + Number,
    {
        let fraction = Option::<T>::deserialize(deserializer)?;

        Ok(fraction.map(|fraction| Percentage::from_fraction(fraction)))
    }

    pub fn serialize<S, T>(
        percentage: &Option<Percentage<T>>,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
        T: Serialize + Number,
    {
        percentage
            .map(|percentage| percentage.to_fraction().into_inner())
            .serialize(serializer)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_f32() {
        let p = Percentage::from_points(125.0_f32);
        assert_eq!(p.to_fraction().into_inner(), 1.25_f32);
    }

    #[test]
    fn test_f64() {
        let p = Percentage::from_points(125.0_f64);
        assert_eq!(p.to_fraction().into_inner(), 1.25_f64);
    }

    #[test]
    fn arithmetic() {
        let a = Percentage::from_points(75.0);
        let b = Percentage::from_fraction(1.25);

        assert_eq!(a + b, Percentage::from_points(200.0));
        assert_eq!(b - a, Percentage::from_points(50.0));
    }

    #[test]
    fn ordering_and_comparison() {
        let a = Percentage::from_points(75.0);
        let b = Percentage::from_fraction(1.25);
        let c = Percentage::from_fraction(0.75);

        assert_eq!(a, c);
        assert_ne!(a, b);
        assert!(a < b);
        assert!(a <= b);
        assert!(b > a);
        assert!(b >= a);
    }

    #[test]
    fn add_subtract_percentage() {
        let base = 100.0_f32;
        let pct = Percentage::from_points(20.0);

        assert_eq!(base + pct, 120.0_f32);
        assert_eq!(base - pct, 80.0_f32);
    }

    #[test]
    fn gain_loss() {
        // Final, Initial, Gain/Loss Points
        let cases = [
            (125.0, 100.0, 25.0),
            (100.0, 125.0, -20.0),
            (75.0, 60.0, 25.0),
            (-30.0, 60.0, -150.0),
        ];

        for (final_value, initial_value, gain_points) in cases {
            let gain_loss = Percentage::gain_loss(final_value, initial_value).unwrap();
            assert_eq!(gain_loss.to_points().into_inner(), gain_points);
        }
    }

    #[test]
    fn part_of_the_whole() {
        // Part, Whole, Percentage Points
        let cases = [(20.0, 80.0, 25.0), (100.0, 125.0, 80.0)];

        for (part, whole, points) in cases {
            let part_of_the_whole = Percentage::part_of_the_whole(part, whole).unwrap();
            assert_eq!(part_of_the_whole.to_points().into_inner(), points);
        }
    }

    #[test]
    fn display() {
        let pct = Percentage::from_fraction(0.123456);
        assert_eq!(format!("{:.1}", pct), "12.3%".to_string());

        // Width applies to the number, and the percent sign is tacked on
        assert_eq!(format!("{:>8.3}", pct), "  12.346%".to_string());
        assert_eq!(format!("{:<8.3}", pct), "12.346  %".to_string());
    }

    #[cfg(feature = "serde")]
    #[test]
    fn serde() {
        use serde::{Deserialize, Serialize};

        #[derive(Debug, PartialEq, Deserialize, Serialize)]
        struct Test {
            #[serde(with = "serde_points")]
            points: Percentage<f64>,
            #[serde(with = "serde_points_maybe")]
            op_points: Option<Percentage<f64>>,
            #[serde(with = "serde_fraction")]
            fraction: Percentage<f64>,
            #[serde(with = "serde_fraction_maybe")]
            op_fraction: Option<Percentage<f64>>,
        }

        let p = Test {
            points: Percentage::from_points(55.0),
            op_points: Some(Percentage::from_points(45.0)),
            fraction: Percentage::from_points(35.0),
            op_fraction: Some(Percentage::from_points(25.0)),
        };
        let ser = serde_json::to_string(&p).unwrap();
        let de: Test = serde_json::from_str(&ser).unwrap();
        assert_eq!(p, de);

        let s = r#"{
            "points": 35.0,
            "op_points": null,
            "fraction": 0.35,
            "op_fraction": null
        }"#;
        let de: Test = serde_json::from_str(s).unwrap();
        assert_eq!(de.points, de.fraction);
        assert!(de.op_points.is_none());
        assert!(de.op_fraction.is_none());
    }
}
