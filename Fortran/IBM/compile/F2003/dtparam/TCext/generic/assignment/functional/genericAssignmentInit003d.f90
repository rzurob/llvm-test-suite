! GB DTP extension using:
! ftcx_dtp -ql -qreuse=base /tstdev/F2003/generic/assignment/functional/genericAssignmentInit003d.f
! opt variations: -qnol -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: initialization expression is not the same as assignment
!*                                           with class hierarchy (generic or intrinsic), and different rank which
!*                                           intrinsic assignment does not support, however we still provide a
!*                                           UD assignment which should not be used
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

   type base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i = -999
      contains
         procedure, pass :: bassgn
         generic :: assignment(=) => bassgn
   end type

   type, extends(base) :: child1    ! (20,4)
      integer(k1) :: j = -999
   end type

   type, extends(base) :: child2    ! (20,4)
      integer(k1) :: k = -999
      contains
         procedure, pass :: bassgn => cassgn
   end type

   contains

      subroutine bassgn ( a, b )
         class(base(*,4)), intent(out) :: a
         class(base(*,4)), intent(in) :: b(:)

         a%i = b(1)%i + 1
         select type ( a )
            type is ( child1(*,4) )
               select type ( b )
                  type is ( child1(*,4) )
                     a%j = b(1)%j + 1
                  type is ( child2(*,4) )
                     a%j = b(1)%k + 1
               end select
         end select

         print *, 'bassgn'

      end subroutine

      subroutine cassgn ( a, b )
         class(child2(*,4)), intent(out) :: a
         class(base(*,4)), intent(in) :: b(:)

         a%i = b(1)%i + 1
         select type ( b )
            type is ( child2(*,4) )
               a%k = b(1)%k + 1
            type is ( child1(*,4) )
               a%k = b(1)%j + 1
         end select
         print *, 'cassgn'

      end subroutine

end module


program genericAssignmentInit003d
   use m

   type(base(20,4)) :: b1 = (/ base(20,4)(100) /)
   type(child1(20,4)) :: c1 = (/ child1(20,4)(200, 400) /)
   type(child2(20,4)) :: c2 = (/ (child2(20,4)(600,800), i = 1, 2 ) /)

end program
