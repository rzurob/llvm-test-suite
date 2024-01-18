! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/operator/functional/genericOperatorPass005.f
! opt variations: -ql

!+  ===================================================================
!+
!+  DATE                       : 11/01/2005
!+                             :
!+
!+  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!+                             :
!+  SECONDARY FUNCTIONS TESTED : with Operator( )
!+
!+  DESCRIPTION                : Binary Operator: with pass attribute with another derived type
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

   type base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i = -999
      contains
         procedure, pass(b) :: int_base1_base
         generic :: operator(+) => int_base1_base
   end type

   type, extends(base) :: child    ! (4)
      contains
         procedure, pass(b) :: int_base1_base => int_base1_child
   end type

   type base1(k2)    ! (4)
      integer, kind :: k2
      integer(k2)   :: i = -999
      contains
         procedure, pass(b) :: int_base_base1
         generic :: operator(+) => int_base_base1
   end type

   type, extends(base1) :: child1    ! (4)
      contains
         procedure, pass(b) :: int_base_base1 => int_base_child1
   end type

   contains

   integer function int_base1_child ( a, b )
      class(base1(4)), intent(in) :: a
      class(child(4)), intent(in) :: b

      int_base1_child = a%i + b%i
      print *, 'int_base1_child: ', int_base1_child

   end function

   integer function int_base1_base ( a, b )
      class(base1(4)), intent(in) :: a
      class(base(4)), intent(in) :: b

      int_base1_base = a%i + b%i
      print *, 'int_base1_base: ', int_base1_base

   end function

   integer function int_base_base1 ( a, b )
      class(base(4)), intent(in) :: a
      class(base1(4)), intent(in) :: b

      int_base_base1 = a%i + b%i
      print *, 'int_base_base1: ', int_base_base1

   end function

   integer function int_base_child1 ( a, b )
      class(base(4)), intent(in)  :: a
      class(child1(4)), intent(in) :: b

      int_base_child1 = a%i + b%i
      print *, 'int_base_child1: ', int_base_child1

   end function

end module

program genericOperatorPass005
   use m

   class(base(4)), allocatable :: b1
   class(base1(4)), allocatable :: b11
   type(child(4)) :: c1
   class(child1(4)), pointer :: c11

   integer :: i, j(3)

   allocate ( b1, source = base(4)( base(4)(10) + base1(4)(20) ) )
   allocate ( b11, source = base1(4)( base1(4)(20) + base(4)(30) ) )

   c1 = child(4)( b1 + b11 + ( child(4)(100) + child1(4)(200) ) )
   allocate ( c11, source = child1(4) ( ( c1 + child1(4)(-100) ) - 20 + ( base1(4)(-10) + child(4)(20) ) ) )

   print *, b1%i, b11%i, c1%i, c11%i

   i = b1 + b11 + 10 + ( c11 + c1 )
   print *,i

   j = base(4)( b1 + c11 ) + child1(4)( c1 + b11 )
   print *,j

   deallocate ( b1, b11 )
   allocate ( b1,source = child(4)(10) )
   allocate ( b11, source =child1(4)(20) )

   i = b1 + b11
   print *,i

   j= b11 + b1
   print *,j

end program
