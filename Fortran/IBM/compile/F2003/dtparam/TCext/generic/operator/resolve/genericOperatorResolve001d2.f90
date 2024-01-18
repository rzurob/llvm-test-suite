! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/operator/resolve/genericOperatorResolve001d2.f
! opt variations: -qnol

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
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

   type base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)      i
      contains
         procedure :: neg
         generic :: operator (-) => neg
   end type

   contains

   type(base(20,4)) function neg (a)
      class(base(*,4)), intent(in) :: a

      neg%i = -1 * a%i
      print *, "neg"
   end function

end module

program genericOperatorResolve001d2
   use m

   interface operator (-)
      type(base(20,4)) elemental function elementalneg (a)
         import base
         class(base(*,4)), intent(in) :: a
      end function
   end interface

end program


type(base(20,4)) elemental function elementalneg (a)
   use m, only: base
   class(base(*,4)), intent(in) :: a

   elementalneg%i = -1 * a%i

end function
