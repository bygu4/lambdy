namespace Lambdy.Lib

module Strategy =

    type Strict = | Strict of bool

    type Strong = | Strong of bool

    type Strategy = | Strategy of Strict * Strong

    let normalOrder = Strategy (Strict false, Strong true)
    let applicativeOrder = Strategy (Strict true, Strong true)
    let callByName = Strategy (Strict false, Strong false)
    let callByValue = Strategy (Strict true, Strong false)
