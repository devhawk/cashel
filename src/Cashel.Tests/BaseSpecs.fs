module BaseSpecs
open NUnit.Framework

let inline (==) (actual:#obj) (expected:#obj) = Assert.AreEqual(expected, actual)
let inline (!=) (actual:#obj) (expected:#obj) = Assert.AreNotEqual(expected, actual)
let inline (<->) (actual:#obj) expected = Assert.IsInstanceOf(expected, actual)
let inline (<!>) (actual:#obj) expected = Assert.IsNotInstanceOf(expected, actual)
let ``is null`` anObject = Assert.IsNull(anObject)
let ``is not null`` anObject = Assert.NotNull(anObject)
let inline (===) (actual:Async<_>) (expected:#obj) =
  let result = Async.RunSynchronously actual
  expected = result |> Assert.IsTrue
let inline (!==) (actual:Async<_>) (expected:#obj) =
  let result = Async.RunSynchronously actual
  expected = result |> Assert.IsFalse