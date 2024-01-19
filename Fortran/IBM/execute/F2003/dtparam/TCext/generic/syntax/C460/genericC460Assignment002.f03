! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/F2003/generic/syntax/C460/genericC460Assignment002.f
! opt variations: -ql -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment( )
!*
!*  DESCRIPTION                : C460: specific-binding exist in parent type
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
         procedure, pass :: myassgn
   end type

   type, extends(base) :: emptychild    ! (4)
      contains
         procedure, pass :: myassgn => childassgn
   end type

   type, extends(emptychild) :: gen3    ! (4)
      integer(k1) :: j = -999
      contains
         generic :: assignment( = ) => myassgn
   end type

   contains

      subroutine myassgn(a, b)
         class(base(4)), intent(out) :: a
         class(base(4)), intent(in)  :: b
         a%i = b%i

         print *, 'baseassgn'

      end subroutine

      subroutine childassgn(a, b)
         class(emptychild(4)), intent(out) :: a
         class(base(4)), intent(in)  :: b

         a%i = b%i

         print *, 'emptychildassgn'

      end subroutine

end module

program genericC460Assignment002
   use m

   type(gen3(4)) :: g1, g2

   g1 = gen3(4)(10, 20)
   g2 = gen3(4)(30, 40)

   g1 = g2

   print *, g1%i, g1%j

end program
