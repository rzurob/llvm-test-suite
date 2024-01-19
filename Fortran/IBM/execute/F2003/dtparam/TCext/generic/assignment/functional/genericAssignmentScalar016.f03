! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/assignment/functional/genericAssignmentScalar016.f
! opt variations: -ql

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: different kind type parameter
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

   type base(k1)    ! (8)
      integer, kind :: k1
      integer(k1)   :: i = -999_8
      contains
         procedure :: assign_8
         procedure :: assign_4
         procedure :: assign_2
         generic :: assignment(=) => assign_8, assign_4, assign_2
   end type

   contains

      subroutine assign_8 ( a, b )
         class(base(8)), intent(out) :: a
         integer(8), intent(in) :: b

         a%i = b
         print *, 'int8'

      end subroutine

      subroutine assign_4 ( a, b )
         class(base(8)), intent(out) :: a
         integer(4), intent(in) :: b

         a%i = INT(b,8)
         print *, 'int4'

      end subroutine

      subroutine assign_2 ( a, b )
         class(base(8)), intent(out) :: a
         integer(2), intent(in) :: b

         a%i = INT(b,8)
         print *, 'int2'

      end subroutine

end module

program genericAssignmentScalar016
   use m

   type(base(8)) :: b1

   b1 = 10
   print *, b1
   b1 = 100_2
   print *, b1
   b1 = 10000_4
   print *, b1
   b1 = 999_8
   print *, b1

   b1 = 10_2 + 20_1
   print *, b1
   b1 = INT(10_2)
   print *, b1

end program
