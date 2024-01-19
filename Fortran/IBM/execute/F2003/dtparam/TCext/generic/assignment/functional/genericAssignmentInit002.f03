! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/F2003/generic/assignment/functional/genericAssignmentInit002.f
! opt variations: -ql -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: initialization expression is not the same as assignment
!*                                           with class hierarchy (generic or intrinsic)
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

   type base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i = -999
      contains
         procedure, pass :: bassgn
         generic :: assignment(=) => bassgn
   end type

   type, extends(base) :: child1    ! (4)
      integer(k1) :: j = -999
   end type

   type, extends(base) :: child2    ! (4)
      integer(k1) :: k = -999
      contains
         procedure, pass :: bassgn => cassgn
   end type

   contains

      subroutine bassgn ( a, b )
         class(base(4)), intent(out) :: a
         class(base(4)), intent(in) :: b

         a%i = b%i + 1
         select type ( a )
            type is ( child1(4) )
               select type ( b )
                  type is ( child1(4) )
                     a%j = b%j + 1
                  type is ( child2(4) )
                     a%j = b%k + 1
               end select
         end select

         print *, 'bassgn'

      end subroutine

      subroutine cassgn ( a, b )
         class(child2(4)), intent(out) :: a
         class(base(4)), intent(in) :: b

         a%i = b%i + 1
         select type ( b )
            type is ( child2(4) )
               a%k = b%k + 1
            type is ( child1(4) )
               a%k = b%j + 1
         end select
         print *, 'cassgn'

      end subroutine

end module


program genericAssignmentInit002
   use m

   type(base(4)) :: b1 = base(4)(100)
   type(child1(4)) :: c1 = child1(4)(200, 300)
   type(child2(4)) :: c2 = child2(4)(400, 500)

   print *, b1
   print *, c1
   print *, c2

   b1 = c1
   c1 = c2
   c2 = b1

   print *, b1
   print *, c1
   print *, c2

end program
