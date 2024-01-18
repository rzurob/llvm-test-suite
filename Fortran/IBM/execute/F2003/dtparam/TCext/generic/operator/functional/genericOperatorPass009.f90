! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/operator/functional/genericOperatorPass009.f
! opt variations: -qnol

!+  ===================================================================
!+  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!+  ===================================================================
!+  ===================================================================
!+
!+  TEST CASE TITLE            :
!+
!+  PROGRAMMER                 : Robert Ma
!+  DATE                       : 11/01/2005
!+  ORIGIN                     : AIX Compiler Development, Toronto Lab
!+                             :
!+
!+  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!+                             :
!+  SECONDARY FUNCTIONS TESTED : with Operator( )
!+
!+  DRIVER STANZA              : xlf95
!+
!+  DESCRIPTION                : Binary Operator: with pass attribute with two different types pointing to same functions (ambiguous in the child type)
!+  KEYWORD(S)                 :
!+  TARGET(S)                  :
!+ ===================================================================
!+
!+  REVISION HISTORY
!+
!+  MM/DD/YY:  Init:  Comments:
!+ ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i = -999
      contains
         procedure, pass(b) :: g => int_base1_base
         generic :: operator(+) => g
   end type

   type, extends(base) :: child    ! (20,4)
      contains
         procedure, pass(b) :: g => int_base1_child
   end type

   type base1(n2,k2)    ! (20,4)
      integer, kind :: k2
      integer, len  :: n2
      integer(k2)   :: j  = -999
      contains
         procedure, pass :: f => int_base1_base
         generic :: operator(+) => f
   end type

   type, extends(base1) :: child1    ! (20,4)
      contains
         procedure, pass :: f => int_child1_base
   end type

   contains

   integer function int_base1_base ( a, b )
      class(base1(*,4)), intent(in) :: a
      class(base(*,4)), intent(in) :: b

      int_base1_base = a%j + b%i

      print *, 'int_base1_base'

   end function

   integer function int_base1_child ( a, b )
      class(base1(*,4)), intent(in) :: a
      class(child(*,4)), intent(in) :: b

      int_base1_child = a%j + b%i
      print *, 'int_base1_child'

   end function

   integer function int_child1_base ( a, b )
      class(child1(*,4)), intent(in) :: a
      class(base(*,4)), intent(in) :: b

      int_child1_base = a%j + b%i
      print *, 'int_child1_base'

   end function

end module

program genericOperatorPass009d
use m
	type(base(20,4)) :: b = base(20,4)(10)
	type(base1(20,4)) :: b1 = base1(20,4)(20)
	type(child(20,4)) :: c = child(20,4)(30)
	type(child1(20,4)) :: c1 = child1(20,4)(60)
	integer :: r
	r = b1+b
    print *,r
	r = b1+c
    print *,r
	r = c1+b
    print *,r
end program
