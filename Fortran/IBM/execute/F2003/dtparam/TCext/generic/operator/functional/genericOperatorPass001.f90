! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/operator/functional/genericOperatorPass001.f
! opt variations: -ql

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Binary Operator: with pass attribute (+)
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
      integer(k1)   :: i = -999
      contains
         procedure, pass    :: base_base_int
         procedure, pass(b) :: base_int_base
         procedure, pass(b) :: base_base_base
         generic :: operator(+) => base_base_int, base_int_base, base_base_base
   end type

   contains

   function base_base_base ( a, b )
      class(base(4)), intent(in) :: a
      class(base(4)), intent(in) :: b

      type(base(4)) :: base_base_base

      base_base_base%i = a%i + b%i
      print *, 'base_base_base'

   end function

   function base_int_base ( a, b )
      integer, intent(in) :: a
      class(base(4)), intent(in)  :: b

      type(base(4)) :: base_int_base

      base_int_base%i = a + b%i
      print *, 'base_int_base'

   end function

   type(base(4)) function base_base_int ( a, b )
      class(base(4)), intent(in) :: a
      integer, intent(in)  :: b

      base_base_int%i = a%i + b
      print *, 'base_base_int'

   end function

end module

program genericOperatorPass001
   use m

   type(base(4)) :: b1
   class(base(4)), allocatable :: b2

   allocate ( b2 , source = base(4)(20))

   b1 = base(4)(5) + 10
   print *,b1%i

   b1 = 50 + base(4)(100)
   print *, b1%i

   b1 = base(4)(100) + 200 + base(4)(300) + 400 + 500 + base(4)(600)
   print *, b1%i

   b1 = b2 + 40 + b1 + 60 + base(4)(100) + 80 + base(4)(40) + 100
   print *, b1%i

end program
