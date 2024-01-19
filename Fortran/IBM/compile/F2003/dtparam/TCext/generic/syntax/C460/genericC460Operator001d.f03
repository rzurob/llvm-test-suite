! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/syntax/C460/genericC460Operator001d.f
! opt variations: -ql

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : C460: specific-binding does not exist
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
      integer(k1)      i
      contains
         generic :: operator(+) => myadd
   end type

   interface myadd
      type(base(4)) function add(a, b)
         import base
         class(base(4)), intent(in) :: a, b
      end function
   end interface

end module

program genericC460Operator001d
end program
