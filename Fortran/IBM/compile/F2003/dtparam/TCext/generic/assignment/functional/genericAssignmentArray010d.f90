! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/assignment/functional/genericAssignmentArray010d.f
! opt variations: -ql

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
!*  DESCRIPTION                : assignment: assumed-size array appearing as whole array, and
!*                                           using generic assignment tb with assumed-size array
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
      integer(k1)   :: i =0
      contains
         procedure, pass :: bassgn
         procedure, pass :: bassgnwrapper
         generic :: assignment(=) => bassgn
   end type

   contains

      subroutine bassgn ( a, b )
         class(base(4)), intent(out) :: a
         class(base(4)), intent(in) :: b(*)

         print *, 'bassgn'

         do j = 1, 5
            a%i = a%i + b(j)%i
         end do

      end subroutine

      subroutine bassgnwrapper ( a, b )
         class(base(4)), intent(out) :: a
         class(base(4)), intent(in) :: b(*)

         a = b

      end subroutine

end module

program genericAssignmentArray010d
end program
