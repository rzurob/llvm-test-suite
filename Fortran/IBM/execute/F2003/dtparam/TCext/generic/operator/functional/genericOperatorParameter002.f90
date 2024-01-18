! GB DTP extension using:
! ftcx_dtp -qck /tstdev/F2003/generic/operator/functional/genericOperatorParameter002.f
! opt variations: -qnock

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
!*  SECONDARY FUNCTIONS TESTED : with Operator
!*
!*  DRIVER STANZA              : xlf2003
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

   type base(k1,n1)    ! (1,3)
      integer, kind             :: k1
      integer, len              :: n1
      character(kind=k1,len=n1) :: c = 'xxx'
      contains
         procedure :: ab
         generic :: operator(//) => ab
   end type


   type, extends( base ) :: child    ! (1,3)
      contains
         procedure :: ab => c
   end type

   contains

   character(6) elemental function ab ( a, b )
      class(base(1,*)), intent(in) :: a, b
      ab = a%c // b%c
   end function

   character(6) elemental  function c ( a, b )
      class(base(1,*)), intent(in) ::  b
      class(child(1,*)), intent(in) ::  a
      c = a%c(1:1) // b%c(1:1) // a%c(2:2) // b%c(2:2) // a%c(3:3) // b%c(3:3)
   end function


end module

program genericOperatorParameter002
   use m

   type(base(1,3)), parameter  :: b1(5) = (/ ( base(1,3)('IBM'), i = 1,5 ) /)
   type(base(1,3)), parameter  :: b2(5) = (/ ( base(1,3)('FTN'), i = 1,5 ) /)
   type(child(1,3)), parameter :: c1(5) = (/ ( child(1,3)('xxx'), i = 1,5 ) /)

   print *, b1 // b2

   print *, c1 // b1

   print *, b2 // c1

   print *, c1 // b2

end program
