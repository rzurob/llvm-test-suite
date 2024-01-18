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
!*  DESCRIPTION                : assignment: private generic assignment should not be accessible outside module
!*                                           private generic tb in base and child types in separate modules
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
         procedure, pass :: assgn => ba
         procedure, pass :: assgnint
         generic, private :: assignment(=) => assgn
   end type

   contains

      subroutine modulemwrapper ( a, b )
         class(base), intent(out) :: a
         class(base), intent(in) :: b

         a = b

      end subroutine

      subroutine ba ( a, b )
         class(base), intent(out) :: a
         class(base), intent(in) :: b

         a%i = b%i
         print *, 'ba'

      end subroutine

      subroutine assgnint ( a, b )
         class(base), intent(out) :: a
         integer, intent(in) :: b

         a%i = b
         print *, 'assgnint'

      end subroutine

   end module

module n
   use m

   type, extends(base) :: child
      contains
         generic, private :: assignment(=) => assgnint
   end type

   contains

      subroutine modulenwrapper ( a, b )
         class(child),intent(out) :: a
         integer, intent(in) :: b

         a = b

      end subroutine

end module

program genericAssignmentScalar011
   use n

   type(base) :: b1
   class(base), pointer :: b2

   type(child) :: c1
   class(child), allocatable :: c2

   b1 = base(10)
   allocate ( b2, c2 )

   call modulemwrapper ( b2, b1 )

   call modulenwrapper ( c1, 30 )

   if ( ( b2%i /= 10 ) .or. ( c1%i /= 30 ) )  error stop 1_4

   call modulemwrapper ( c2, c1 )

   call modulemwrapper ( c1, b1 )

   if ( ( c1%i /= 10 ) .or. ( c2%i /= 30 ) )  error stop 2_4

end program
