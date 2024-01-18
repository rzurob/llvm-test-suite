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

   type base
      integer :: i = -999
      contains
         procedure, pass(b) :: int_base1_base
         generic :: operator(+) => int_base1_base
   end type

   type, extends(base) :: child
      contains
         procedure, pass(b) :: int_base1_base => int_base1_child
   end type

   type base1
      integer :: i = -999
      contains
         procedure, pass(b) :: int_base_base1
         generic :: operator(+) => int_base_base1
   end type

   type, extends(base1) :: child1
      contains
         procedure, pass(b) :: int_base_base1 => int_base_child1
   end type

   contains

   integer function int_base1_child ( a, b )
      class(base1), intent(in) :: a
      class(child), intent(in) :: b

      int_base1_child = a%i + b%i
      print *, 'int_base1_child: ', int_base1_child

   end function

   integer function int_base1_base ( a, b )
      class(base1), intent(in) :: a
      class(base), intent(in) :: b

      int_base1_base = a%i + b%i
      print *, 'int_base1_base: ', int_base1_base

   end function

   integer function int_base_base1 ( a, b )
      class(base), intent(in) :: a
      class(base1), intent(in) :: b

      int_base_base1 = a%i + b%i
      print *, 'int_base_base1: ', int_base_base1

   end function

   integer function int_base_child1 ( a, b )
      class(base), intent(in)  :: a
      class(child1), intent(in) :: b

      int_base_child1 = a%i + b%i
      print *, 'int_base_child1: ', int_base_child1

   end function

end module

program genericOperatorPass005
   use m

   class(base), allocatable :: b1
   class(base1), allocatable :: b11
   type(child) :: c1
   class(child1), pointer :: c11

   integer :: i, j(3)

   allocate ( b1, source = base( base(10) + base1(20) ) )
   allocate ( b11, source = base1( base1(20) + base(30) ) )

   c1 = child( b1 + b11 + ( child(100) + child1(200) ) )
   allocate ( c11, source = child1 ( ( c1 + child1(-100) ) - 20 + ( base1(-10) + child(20) ) ) )

   print *, b1%i, b11%i, c1%i, c11%i

   i = b1 + b11 + 10 + ( c11 + c1 )
   print *,i

   j = base( b1 + c11 ) + child1( c1 + b11 )
   print *,j

   deallocate ( b1, b11 )
   allocate ( b1,source = child(10) )
   allocate ( b11, source =child1(20) )

   i = b1 + b11
   print *,i

   j= b11 + b1
   print *,j

end program
