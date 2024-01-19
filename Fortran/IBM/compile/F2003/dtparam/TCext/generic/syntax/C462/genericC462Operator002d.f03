! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/generic/syntax/C462/genericC462Operator002d.f
! opt variations: -qck -qnok -ql

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : C462:
!*                               Section 12.3.2.1.1 - Defined Operations
!(                                  non intent(in) dummy arguments
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

   type :: base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)      i
      contains
         procedure, pass :: mypower
         generic :: operator(**) => mypower
   end type

   type :: base2(k2,n1)    ! (4,3)
      integer, kind :: k2
      integer, len  :: n1
      character(n1) :: c
      contains
         procedure, pass :: concat
         generic :: operator(//) => concat

         procedure, pass :: not
         generic :: operator(.not.) => not

   end type

   contains

   function mypower ( a, b )
      class(base(4)), intent(inout) :: a
      class(base(4)), intent(out) :: b
      type(base(4)) :: mypower

      mypower%i = a%i ** b%i
   end function

   function concat ( a, b )
      class(base2(4,*)), intent(inout) :: a
      class(base2(4,*)), intent(in)    :: b
      type(base2(4,3)) :: concat

      concat%c = 'ab'//'C'
   end function

   function not ( a )
      class(base2(4,*)), intent(out) :: a
      type(base2(4,3)) :: not

      not%c = 'xxx'

   end function

end module

end
