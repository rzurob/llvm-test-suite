! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/misc/genericMisc003.f
! opt variations: -qnol

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Misc.
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Specific type bound procedure with PASS (arg-name) and PRIVATE
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i = -999
      contains
         procedure, pass(a), private :: b
   end type

   contains

   subroutine b ( a )
      class(base(*,4)), intent(in) :: a

      print *, a%i

   end subroutine

   subroutine foo (a)
      class(base(*,4)), intent(in) :: a
      call a%b
   end subroutine

end module

program genericMisc003
   use m

   type(base(20,4)) :: b1 = base(20,4)(10)
   call foo(b1)

end program
