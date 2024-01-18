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
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : assignment: component that has generic assignment tb as well
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
      integer :: i = -999
      contains
         procedure, private :: innerassignment
         generic, private :: assignment(=) => innerassignment
   end type

   type base
      type(inner) :: i
      contains
         generic :: assignment(=) => assignment
         procedure :: assignment
   end type

   contains

   subroutine innerassignment ( a, b)
      class(inner), intent(out) :: a
      type(inner), intent(in) :: b

      print *, 'innerassgn'
      a%i = b%i

   end subroutine

   subroutine assignment ( a, b)
      class(base), intent(out) :: a
      class(base), intent(in) :: b

      print *, 'assign'
      a%i = b%i

   end subroutine

end module

program genericAssignmentScalar017
   use m

   type(base), pointer :: b1
   class(base), allocatable :: b2

   allocate ( b1, b2 )

   b1 = base( inner(10) )
   b2 = b1

   if ( ( b1%i%i /= 10 ) .or. ( b2%i%i /= 10 ) ) error stop 1_4

   print *, 'end'
   b1%i = inner(20) !<- should not use generic assignment
   b2%i = b1%i

   if ( ( b1%i%i /= 20 ) .or. ( b2%i%i /= 20 ) ) error stop 2_4

end program
