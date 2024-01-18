!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Misc.
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
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

   type base
      integer :: i = -999
      contains
         procedure, pass(a), private :: b
   end type

   contains

   subroutine b ( a )
      class(base), intent(in) :: a
      
      print *, a%i
      
   end subroutine
   
   subroutine foo (a)
      class(base), intent(in) :: a
      call a%b
   end subroutine

end module

program genericMisc003
   use m

   type(base) :: b1 = base(10)
   call foo(b1)

end program
