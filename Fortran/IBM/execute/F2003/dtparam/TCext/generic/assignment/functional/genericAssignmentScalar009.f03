! GB DTP extension using:
! ftcx_dtp -ql -qreuse=base /tstdev/F2003/generic/assignment/functional/genericAssignmentScalar009.f
! opt variations: -qnol -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: private generic assignment should not be accessible outside module
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
         procedure, pass :: assgn => ba
         generic, private :: assignment(=) => assgn
   end type


   type, extends(base) :: child    ! (20,4)
      integer(k1) :: j = -999
      contains
         procedure, pass :: assgn => ca
   end type

   contains

      subroutine ba ( a, b )
         class(base(*,4)), intent(out) :: a
         class(base(*,4)), intent(in) :: b

         a%i = b%i
         print *, 'ba'

      end subroutine

      subroutine ca ( a, b )
         class(child(*,4)), intent(out) :: a
         class(base(*,4)), intent(in) :: b

         a%i = b%i

         print *, 'ca'

         select type ( b )
            type is ( child(*,4) )
               a%j = b%j
         end select

      end subroutine

end module


program genericAssignmentScalar009
   use m

   type(base(20,4)) :: b1, b2

   b1 = base(20,4)(10)  !<- ensure generic type bound is not called
   b2 = base(20,4)(20)  !<- ensure generic type bound is not called

   b1 = b2        !<- ensure generic type bound is not called

   if ( ( b1%i /= 20 ) .or. ( b2%i /= 20 ) )  error stop 1_4

end program
