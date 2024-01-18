!*  ===================================================================
!*
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Diagnostic test
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Testing the function argument which has
!*                               deferred type parameter.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
module m
type Person
   character(:), pointer :: name
end type

interface
   function fun(x)
      import Person
      type (Person) :: x
      character(4), allocatable :: fun
   end function
end interface
end module

use m
type (Person) p
character(:), target, allocatable :: char
character*4 result
allocate(character(4)::char)
char = 'John'
p%name => char

result = fun(p)
if (result /= "John") error stop 1

deallocate(char)

end

function fun(x)
   use m
   type (Person) :: x
   character(4), allocatable :: fun
   allocate(fun)
   fun = x%name
end
