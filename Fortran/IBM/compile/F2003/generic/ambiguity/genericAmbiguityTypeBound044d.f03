!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DESCRIPTION                : deferred binding using same interface, but
!*                               child type points to different procedures
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module genericName

   type, abstract :: b1
      integer :: i
      contains
         procedure(firstsub), deferred, pass(a) :: twoargs1
         procedure(firstsub), deferred, pass(a) :: twoargs2
         generic :: twoargs => twoargs1, twoargs2
   end type

   abstract interface
      subroutine firstsub(a, b)
         import b1
         class(b1), intent(in) :: a
         class(b1), intent(in) :: b
      end subroutine
   end interface

   type, extends(b1) :: c1
      contains
         procedure, pass(a) :: twoargs1
         procedure, pass(a) :: twoargs2
   end type

   contains

      subroutine twoargs1(a, b)
         class(c1), intent(in) :: a
         class(b1), intent(in) :: b

      end subroutine

      subroutine twoargs2(a, b)
         class(c1), intent(in) :: a
         class(b1), intent(in) :: b

      end subroutine

end module

program genericAmbiguityTypeBound044d
end program
