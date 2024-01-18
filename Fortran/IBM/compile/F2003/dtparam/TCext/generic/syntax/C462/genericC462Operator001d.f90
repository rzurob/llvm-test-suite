! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=base /tstdev/F2003/generic/syntax/C462/genericC462Operator001d.f
! opt variations: -qck -qnok -qnol -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : C462:
!*                               Section 12.3.2.1.1 - Defined Operations
!(                                  more than two arguments (no arguments require nopass, therefore no need to test)
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

   type, abstract :: abstractbase(k1,n1)    ! (4,20)
       integer, kind :: k1
       integer, len  :: n1
   end type

   type, extends(abstractbase) :: base    ! (4,20)
      integer(k1) i
      contains
         procedure, pass :: mypower
         generic :: operator(**) => mypower
   end type

   type, extends(abstractbase) :: base2(n2)    ! (4,20,3)
      integer, len  :: n2
      character(n2) :: c
      contains
         procedure, pass :: concat
         generic :: operator(//) => concat

         procedure, pass :: not
         generic :: operator(.not.) => not

   end type

   contains

   function mypower ( a, b, c )
      class(base(4,*)), intent(in) :: a, b, c   ! more than two arguments
      type(base(4,20)) :: mypower

      mypower%i = a%i ** b%i + c%i
   end function

   function concat ( a, b, c )
      class(base2(4,*,*)), intent(in) :: a
      class(base2(4,*,*)), intent(in), optional :: b,c  ! more than two arguments, and the last one is optional
      type(base2(4,20,3)) :: concat

      concat%c = 'ab'//'C'
   end function

   function not ( a, b ) ! unary operator
      class(base2(4,*,*)), intent(in) :: a, b ! more than one arguments for unary operator
      type(base2(4,20,3)) :: not
      not%c = 'xxx'
   end function

end module

end
