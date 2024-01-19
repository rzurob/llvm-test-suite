!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: non-polymorphic elemental assignment for type component as well as type
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

   type inner
      integer :: i
      contains
         procedure, pass :: iassgn
         generic :: assignment(=) => iassgn
   end type

   type base
      type(inner) :: in(3)
      contains
         procedure, pass :: bassgn
         generic :: assignment(=) => bassgn
   end type

   contains

      elemental subroutine iassgn ( a, b )
         class(inner), intent(inout) :: a
         type(inner), intent(in) :: b

         a%i = b%i + 1

      end subroutine

      elemental subroutine bassgn ( a, b )
         class(base), intent(inout) :: a
         type(base), intent(in) :: b

         a%in = b%in

      end subroutine

end module

program genericAssignmentElemental004
   use m

   type(base) :: b1
   type(base) :: b2(3)
   type(base), allocatable :: b3(:)

   b1 = base(inner(100))
   print *, b1

   b2 = base(inner(200))
   print *, b2

   allocate ( b3(5) )
   b3 = base(inner(300))
   print *, b3

   b2 = (/ base(inner(2000)), base(inner(2001)), base(inner(2002)) /)
   print *, b2

   b3 = (/ b2, base( (/inner(2004), inner(2005), inner(2006)/) ), base( (/inner(2007), inner(2008), inner(2009)/) ) /)
   print *, b3

end program
