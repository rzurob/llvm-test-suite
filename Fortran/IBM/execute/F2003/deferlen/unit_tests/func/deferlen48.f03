!*  ===================================================================
!*
!*  PRIMARY FUNCTIONS TESTED   : Unit testing
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  DESCRIPTION                : Testing the deferred length character
!*                               with type-bind procedure.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type student
      character(:), allocatable :: name
      contains
         procedure :: ptr => fun
   end type
contains
function fun(arg)
   class(student) :: arg
   character(:), allocatable :: fun

   allocate(fun, source=arg%name)
end function fun
end module m

program main
use m

   type(student) :: s1, s2, s3
   allocate(character(8)::s2%name)
   allocate(character(8)::s3%name)
   s2%name = 'George'
   s1 = s2
   if (s1%name /= 'George') error stop 1
   s3%name = s1%ptr()
   if (s3%name /= 'George') error stop 2

end
