! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/syntax/C703/genericC703OpName001.f
! opt variations: -qnol

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment( )
!*
!*  DESCRIPTION                : C703: unary generic operator name has to be less than 63 characters long
!*                                     exactly 63 letters long
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
      integer(k1)      x
      contains
         generic :: operator(.abcdefghiJabcdefghiJabcdefghiJabcdefghiJabcdefghiJabcdefghiJabc.) => myunaryop
         procedure :: myunaryop
   end type

   contains

      type(base(20,4)) function myunaryop ( b )
         class(base(*,4)), intent(in) :: b

         myunaryop%x = -1* b%x
      end function

end module

program genericC703OpName001
   use m

   type(base(20,4)) :: a, b

   a = base(20,4)(10)
   b = base(20,4)(20)

   a = .abcdefghiJabcdefghiJabcdefghiJabcdefghiJabcdefghiJabcdefghiJabc. b
   if (a%x /= -20) error stop 10
end