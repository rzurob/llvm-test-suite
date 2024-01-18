! GB DTP extension using:
! ftcx_dtp -qck -qk -qnol /tstdev/F2003/deferlen/unit_tests/func/deferlen27.f
! opt variations: -qnock -qnok -ql

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Deferred Character Length
!*
!*  PROGRAMMER                 : James Ren
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Diagnostic test
!*
!*  DRIVER STANZA              : xlf90/95
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Testing the function argument which has 
!*                               deferred type parameter. 
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
module m
type Person(k1)    ! (4)
    integer, kind :: k1
   character(:), pointer :: name
end type   

interface
   function fun(x)
      import Person
      type (Person(4)) :: x
      character(4), allocatable :: fun
   end function
end interface
end module

use m
type (Person(4)) p
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
   type (Person(4)) :: x
   character(4), allocatable :: fun
   allocate(fun)
   fun = x%name
end
