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
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : generic-name: scalar derived type calling
!*                                             generic binding containing some private/public and some pass/nopass bindings
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
      integer :: i
      contains
         procedure, private, pass(a) :: oneargA
         procedure, public , nopass  :: oneargnp
         procedure, pass(a) :: twoargA
         procedure, private, nopass :: twoargnp
         procedure, pass(b) :: twoargB

         generic :: mygeneric => oneargA, oneargnp, twoargnp, twoargA, twoargB
   end type

   contains

   subroutine oneargA(a)
      class(base), intent(in) :: a
      print *, 'oneargA:', a%i
   end subroutine

   subroutine oneargnp(i)
      integer, intent(in) :: i
      print *, 'oneargnp:', i
   end subroutine

   subroutine twoargA(a, b)
      class(base), intent(in) :: a
      class(base), intent(in) :: b

      print *, 'twoargA:', a%i, b%i

   end subroutine

   subroutine twoargB(a, b)
      integer(8), intent(in) :: a
      class(base), intent(in) :: b

      print *, 'twoargB:', a, b%i

   end subroutine

   subroutine twoargnp(a, b)
      integer, intent(in) :: a
      integer, intent(in) :: b

      print *, 'twoargnp:', a, b

   end subroutine

end module


program genericGenericNameScalar012
   use m

   type(base) :: b1 = base(1000)

   call b1%mygeneric()
   call b1%mygeneric(100)
   call b1%mygeneric(base(2000))
   call b1%mygeneric(200_8)
   call b1%mygeneric(300,400)

end program
