!*  ===================================================================
!*
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Functional test
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : The procedure pointer which has the same
!*                               interface as an abstract interface can
!*                               point to any procedures as long as they
!*                               have the same interface
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
module m
abstract interface
   subroutine sub(arg1, arg2)
      integer, intent(inout) :: arg1, arg2
   end subroutine
end interface
end module

use m
integer a, b
procedure (sub) swap
procedure (sub), pointer :: p
a = 10
b = 20
p=>swap
call p(a, b)
if ((a .ne. 20 ) .or. (b .ne. 10)) then
   error stop 1
endif
end program

subroutine swap(a, b)
   integer, intent(inout) :: a, b, c
   c = a
   a = b
   b = c
return
end subroutine
