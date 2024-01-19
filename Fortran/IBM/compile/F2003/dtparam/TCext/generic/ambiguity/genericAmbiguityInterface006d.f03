! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/ambiguity/genericAmbiguityInterface006d.f
! opt variations: -qnol

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DESCRIPTION                : generic interface with name the same as types
!*
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
      integer(k1)   :: i, j, k
   end type

   interface base

      procedure base1_1arg

      type(base(20,4)) function base1_2args (i,j,k)
        import base
         integer, intent(in) :: i, j, k
         optional :: k
      end function

   end interface

   contains

      type(base(20,4)) function base1_1arg (i)
         integer, intent(in) :: i
         base1_1arg%i = i
         base1_1arg%j = i
         base1_1arg%k = i
      end function

      type(base(20,4)) function base1_3args (i,j,k)
         integer, intent(in) :: i,j,k
         base1_3args%i = i
         base1_3args%j = j
         base1_3args%k = k
      end function

end module

type(base(20,4)) function base1_2args (i,j,k)
   use m, only: base
   integer, intent(in) :: i, j, k
   optional :: k

   base1_2args%i = i
   base1_2args%j = j
   base1_2args%k = k

end function

program genericAmbiguityInterface006d
   use m

   interface base
      procedure base1_3args
   end interface

end program
