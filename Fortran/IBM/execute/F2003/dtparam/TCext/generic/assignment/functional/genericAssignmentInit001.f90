! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/assignment/functional/genericAssignmentInit001.f
! opt variations: -qnol

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: initialization expression is not the same as assignment (generic or intrinsic)
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
      integer(k1)   :: i
      contains
         procedure, pass :: bassgn
         generic :: assignment(=) => bassgn
   end type

   contains

      subroutine bassgn ( a, b )
         class(base(*,4)), intent(out) :: a
         type(base(*,4)), intent(in) :: b

         a%i = b%i + 1
         print *, a%i, '=', b%i

      end subroutine

end module


program genericAssignmentInit001
   use m

   type(base(20,4)) :: b1 = base(20,4)(10)  !<- initialization is not the same as assignment
                                ! (user-defined assignment should not be used)
   print *, b1%i
   b1 = base(20,4)(20)

   print *, b1%i

end program
