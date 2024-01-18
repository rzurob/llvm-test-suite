! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/operator/resolve/genericOperatorResolve002d2.f
! opt variations: -qnol

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: 12.4.5 Resolving type-bound procedure references
!*                                         ii ) contains both scalar and elemental references for binary op in interface
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
         procedure :: add
         generic :: operator (+) => add
   end type

   contains

   type(base(20,4)) function add (a,b)
      class(base(*,4)), intent(in) :: a, b

      add%i = a%i + b%i

   end function


end module

type(base(20,4)) elemental function elementaladd (a,b)
   use m, only: base
   class(base(*,4)), intent(in) :: a, b

   elementaladd%i = a%i + b%i

end function

module n
   use m

   interface operator(+)
      type(base(20,4)) elemental function elementaladd (a,b)
         import base
         class(base(*,4)), intent(in) :: a, b
      end function
   end interface

end module

program genericOperatorResolve002d2
end program
