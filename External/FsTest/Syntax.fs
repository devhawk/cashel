#light

namespace FsxUnit

module Syntax =
    open Xunit
    
    let be (f : 'a -> unit) actual =
      f actual
    
    let False condition =
      Assert.False(condition)
    
    let True condition =
      Assert.True(condition)
      
    let Empty collection =
      Assert.Empty(collection)
      
    let NonEmpty collection =
      Assert.NotEmpty(collection)
      
    let Null item =
      Assert.Null(item)
      
    let NonNull item =
      Assert.NotNull(item)
      
    let equal expected actual =
      Assert.Equal(expected, actual)
    
    let not_equal expected actual =
      Assert.NotEqual(expected, actual)
    
    let contain expected actual =
      Assert.Contains((expected:string), (actual:string))
      
    let not_contain expected actual =
      Assert.DoesNotContain((expected:string), (actual:string))
    
    let have expected actual =
      Assert.Contains(expected, (actual :> seq<_>))
      
    let not_have expected actual =
      Assert.DoesNotContain(expected, (actual :> seq<_>))
      
    let be_same_as expected actual =
      Assert.Same(expected, actual)
      
    let not_be_same_as expected actual =
      Assert.NotSame(expected, actual)
      
    let be_in_range low high actual =
      Assert.InRange(actual, low, high)
      
    let not_be_in_range low high actual =
      Assert.NotInRange(actual, low, high)
      
    let be_type (t:System.Type) (actual:'a) =
      Assert.IsType(t, actual)
      
    let be_assignable_from (t:System.Type) (actual:'a) =
      Assert.IsAssignableFrom(t, actual)
        
    let throw_exception<'a when 'a :> exn> actual =
      Assert.Throws<'a>(Assert.ThrowsDelegate(actual))
      
    let not_throw_exception actual =
      Assert.DoesNotThrow(Assert.ThrowsDelegate(actual))
     
    let should f actual =
        f actual