! GB DTP extension using:
! ftcx_dtp -qk /tstdev/F2003/generic/operator/functional/genericOperatorParameter001.f
! opt variations: -qck -qnok

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator
!*
!*  DESCRIPTION                : Operator: named-constant (parameter) should still invoke the generic tb procedures
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

   type base(k1,n1)    ! (4,3)
      integer, kind :: k1
      integer, len  :: n1
      character(n1) :: c = 'xxx'
      contains
         procedure :: ab
         generic :: operator(//) => ab
   end type


   type, extends( base ) :: child    ! (4,3)
      contains
         procedure :: ab => c
         generic :: operator(//) => ab !<- specifying the same binding name twice
   end type

   contains

   character(6) function ab ( a, b )
      class(base(4,*)), intent(in) :: a, b
      ab = a%c // b%c
   end function

   character(6) function c ( a, b )
      class(base(4,*)), intent(in) ::  b
      class(child(4,*)), intent(in) ::  a
      c = a%c(1:1) // b%c(1:1) // a%c(2:2) // b%c(2:2) // a%c(3:3) // b%c(3:3)
   end function


end module

program genericOperatorParameter001
   use m

   type(base(4,3)), parameter  :: b1 = base(4,3)('IBM')
   type(base(4,3)), parameter  :: b2 = base(4,3)('FTN')
   type(child(4,3)), parameter :: c1 = child(4,3)('xxx')

   print *, b1 // b2

   print *, c1 // b1

   print *, b2 // c1

   print *, c1 // b2

end program
