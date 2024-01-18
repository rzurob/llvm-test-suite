! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/operator/resolve/genericOperatorResolve001d1.f
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
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Operator: 12.4.5 Resolving type-bound procedure references
!*                                         i ) contains both scalar and elemental references for unary op with interface
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

   type base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)      i
      contains
         procedure :: neg
         generic :: operator (-) => neg
   end type

   interface operator (-)
      module procedure elementalneg
   end interface

   contains

   type(base(4)) function neg (a)
      class(base(4)), intent(in) :: a

      neg%i = -1 * a%i

   end function

   type(base(4)) elemental function elementalneg (a)
      class(base(4)), intent(in) :: a

      elementalneg%i = -1 * a%i

   end function

end module

program genericOperatorResolve001d1
end program
