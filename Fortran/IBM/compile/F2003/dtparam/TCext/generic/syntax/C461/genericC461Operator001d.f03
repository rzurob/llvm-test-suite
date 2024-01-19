! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/syntax/C461/genericC461Operator001d.f
! opt variations: -qnol

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : C461: Generic type bound with operator and do not
!*                                     specify pass object dummy argument (binary operator)
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
      integer(k1)   :: i = -1
   contains
      procedure, nopass, private :: add
      generic :: operator(+) => add, addwithint
      procedure, nopass :: addwithint
   end type

   contains

      type(base(20,4)) function add ( a, b )
         class(base(*,4)), intent(in) :: a
         class(base(*,4)), intent(in) :: b

         add%i = a%i + b%i

      end function

      type(base(20,4)) function addwithint ( a, b )
         class(base(*,4)), intent(in) :: a
         integer , intent(in) :: b

         addwithint%i = a%i + b

      end function

end module


program genericC461Operator001d
end program
