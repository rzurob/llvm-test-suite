! GB DTP extension using:
! ftcx_dtp -qck -qnol /tstdev/F2003/generic/syntax/C461/genericC461Operator002d.f
! opt variations: -qnock -ql

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : GENERICS
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
!*  DESCRIPTION                : C461: Generic type bound with operator and do not
!*                                     specify pass object dummy argument (unary operator)
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
      integer(k1)   :: i = -1
   contains
      procedure, nopass, private :: not
      generic :: operator(.not.) => not
   end type

   type, extends(base) :: child(k2,n1)    ! (4,1,3)
      integer, kind             :: k2
      integer, len              :: n1
      character(kind=k2,len=n1) :: c = 'xxx'
      contains
         procedure, nopass :: childnot
         generic :: operator(.not.) => childnot
   end type

   contains

      type(base(4)) function not ( a )
         class(base(4)), intent(in) :: a

         not%i = -1 * a%i

      end function

      type(child(4,1,3)) function childnot ( a )
         type(child(4,1,*)), intent(in) :: a

         childnot%base = .not. a%base
         childnot%c = 'xxx'

      end function

end module


program genericC461Operator002d
end program
