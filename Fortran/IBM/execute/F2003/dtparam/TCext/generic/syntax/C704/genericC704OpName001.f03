! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/syntax/C704/genericC704OpName001.f
! opt variations: -qnol

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment( )
!*
!*  DESCRIPTION                : C703: Binary generic operator name has to be less than 63 characters long
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
         generic :: operator(.abcdefghiJabcdefghiJabcdefghiJabcdefghiJabcdefghiJabcdefghiJabc.) => mybinaryop
         procedure :: mybinaryop
   end type

   contains

      type(base(20,4)) function mybinaryop ( a, b )
         class(base(*,4)), intent(in) :: a, b

         mybinaryop%x = a%x + b%x
      end function

end module

program genericC704OpName001
   use m

   type(base(20,4)) :: a, b

   a = base(20,4)(10)
   b = base(20,4)(20)

   a = a .abcdefghiJabcdefghiJabcdefghiJabcdefghiJabcdefghiJabcdefghiJabc. b

end