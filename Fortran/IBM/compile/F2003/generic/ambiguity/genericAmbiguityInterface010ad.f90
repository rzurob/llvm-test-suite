!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DESCRIPTION                : ambiguous but between interface and deferred type bound
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

module assignment

   type, abstract :: base2
      contains
         procedure(assgn2), deferred, pass :: assgn1
         generic :: assignment(=) => assgn1
   end type

   type, extends(base2) :: child2
      contains
         procedure, pass :: assgn1
   end type

   interface assignment(=)
      subroutine assgn2(a, b)
         import base2
         class(base2), intent(out) :: a
         class(base2), intent(in)  :: b
      end subroutine
   end interface

   interface
      subroutine assgn1(a, b)
         import base2, child2
         class(child2), intent(out) :: a
         class(base2), intent(in)  :: b
      end subroutine
   end interface

end module

subroutine assgn1(a, b)
   use assignment, only: child2, base2
   class(child2), intent(out) :: a
   class(base2), intent(in)  :: b
end subroutine

subroutine assgn2(a, b)
   use assignment, only: base2
   class(base2), intent(out) :: a
   class(base2), intent(in)  :: b
end subroutine

program genericAmbiguityInterface010ad
end program

