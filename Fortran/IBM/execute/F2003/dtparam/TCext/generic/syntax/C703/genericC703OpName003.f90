! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/syntax/C703/genericC703OpName003.f
! opt variations: -qnol

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
!*  SECONDARY FUNCTIONS TESTED : with Assignment( )
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : C703: unary generic operator name has to be less than 63 characters long
!*                                     more than 63 letters
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
         generic :: operator(.abcdefghiJabcdefghiJabcdefghiJabcdefghiJabcdefghiJabcdefghiJabcD.) => myunaryop
         procedure :: myunaryop
   end type

   contains

   type(base(20,4)) function myunaryop ( b )
      class(base(*,4)), intent(in) :: b
      myunaryop%x = -1* b%x
   end function

end module

program genericC703OpName003
end
