module BaseSpecs
open NUnit.Framework

let inline (==) (actual:#obj) (expected:#obj) = Assert.AreEqual(expected, actual)
let inline (!=) (actual:#obj) (expected:#obj) = Assert.AreNotEqual(expected, actual)
let inline (<->) (actual:#obj) expected = Assert.IsInstanceOf(expected, actual)
let inline (<!>) (actual:#obj) expected = Assert.IsNotInstanceOf(expected, actual)
let ``is null`` anObject = Assert.IsNull(anObject)
let ``is not null`` anObject = Assert.NotNull(anObject)

let inline (===) actual expected =
  expected = actual
  |> (fun res -> Assert.IsTrue(res, sprintf "Expected: %A\r\n  But was: %A." expected actual))
let inline (!==) actual expected =
  expected = actual
  |> (fun res -> Assert.IsTrue(res, sprintf "Did not expect: %A\r\n  But was: %A." expected actual))
