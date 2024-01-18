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
!+  DESCRIPTION                : Binary Operator: with pass attribute with two different types pointing to same functions
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
         procedure, pass(b) :: g => int_base1_base
         generic :: operator(+) => g
   end type

   type base1
      integer :: j  = -999
      contains
         procedure, pass :: f => int_base1_base
         generic :: operator(+) => f
   end type

   contains

   integer function int_base1_base ( a, b )
      class(base1), intent(in) :: a
      class(base), intent(in) :: b

      int_base1_base = a%j + b%i
   end function

end module

program genericOperatorPass008
   use m

   class(base), allocatable :: b1
   class(base1), pointer :: b11

   allocate ( b1, source = base ( base1(10) + base(20) ) )
   allocate ( b11, source = base1( base1(30) + base(40) ) )

   print *, b11+b1
   print *, base1(b11+b1) + base(base1(100) +base(200))

end program
