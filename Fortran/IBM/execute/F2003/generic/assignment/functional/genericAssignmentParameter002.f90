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
!*  SECONDARY FUNCTIONS TESTED : with assignment
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : assignment: named-constant (parameter) should still invoke the generic tb procedures
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
         procedure :: ab
         generic :: assignment(=) => ab
   end type

   type, extends( base ) :: child
      contains
         procedure :: c
         generic :: assignment(=) => c
   end type

   contains

   elemental subroutine ab ( a, b )
      class(base), intent(out) :: a
      class(base), intent(in) :: b
      a%i = b%i
   end subroutine

   subroutine c ( a, b )
      class(child), intent(out) :: a
      class(base), intent(in) :: b(:)

      a%i = b(1)%i
      do i = 2, size ( b )
         a%i = a%i + b(i)%i
      end do
   end subroutine


end module

program genericAssignmentParameter002
   use m

   type(base), parameter :: b1(4) = (/ ( base(10*i), i = 1, 4 ) /)
   type(child), parameter :: c1(4) = (/ ( child(100*i), i = 1, 4 ) /)
   
   class(base), allocatable :: b2, b3(:)
   class(child), pointer :: c2, c3(:)

   allocate ( b2, c2, b3(4), c3(4) )

   b2 = b1(4)
   print *, b2%i

   c2 = c1
   print *, c2%i

   b3 = b1
   print *, b3%i

   c3 = c1
   print *, c3%i

   deallocate ( b3 )
   allocate ( child :: b3(4) )

   b3 = c1
   print *, b3%i

end program
