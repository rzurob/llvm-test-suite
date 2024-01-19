! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/F2003/generic/assignment/functional/genericAssignmentScalar010.f
! opt variations: -ql -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: private generic assignment should not be accessible outside module
!*                                           with a public interface wrapped around the private one
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i = -999
      contains
         procedure, pass :: assgn => ba
         generic, private :: assignment(=) => assgn
   end type

   type, extends(base) :: child    ! (4)
      integer(k1) :: j = -999
      contains
         procedure, pass ::  assgn => ca
   end type

   interface assignment(=)
      module procedure publicba
   end interface

   contains

      subroutine publicba ( a, b )
         class(base(4)), intent(out) :: a
         integer, intent(in) :: b

         a = base(4)(b)
         print *, 'publicba'

      end subroutine

      subroutine ba ( a, b )
         class(base(4)), intent(out) :: a
         class(base(4)), intent(in) :: b

         a%i = b%i
         print *, 'ba'

      end subroutine

      subroutine ca ( a, b )
         class(child(4)), intent(out) :: a
         class(base(4)), intent(in) :: b

         a%i = b%i

         print *, 'ca'

         select type ( b )
            type is ( child(4) )
               a%j = b%j
         end select

      end subroutine

end module

program genericAssignmentScalar010
   use m

   type(base(4)) :: b1
   class(base(4)), allocatable :: b2
   type(child(4)) :: c1
   class(child(4)), pointer :: c2

   allocate ( b2, c2 )

   b1 = 10
   b2 = 20
   c1 = 30
   c2 = 40

   if ( ( b1%i /= 10 ) .or. ( b2%i /= 20 ) .or. ( c1%i /= 30 ) .or. ( c2%i /= 40 ) ) error stop 1_4
   deallocate ( b2 )

   allocate ( child(4) :: b2 )
   b2 = 50
   if ( b2%i /= 50 ) error stop 2_4

end program
