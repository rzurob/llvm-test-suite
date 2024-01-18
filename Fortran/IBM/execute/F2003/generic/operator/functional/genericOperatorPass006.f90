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
!+  DESCRIPTION                : Binary Operator: with pass attribute with unlimited poly dummy arg
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
         procedure, pass(b) :: int_U_base
         generic :: operator(+) => int_U_base
   end type

   type, extends(base) :: child
   end type

   contains

   integer function int_U_base ( a, b )
      class(*), intent(in) :: a
      class(base), intent(in) :: b

      select type ( a )
         type is ( integer )
            int_U_base = a + b%i
         type is ( real )
            int_U_base = int(a, kind(b%i) ) + b%i
         class is ( base )
            int_U_base = a%i + b%i
      end select
   end function

end module

program genericOperatorPass006
   use m

   class(base), pointer :: b1, b2

   allocate ( b2, source = base( 10 + base(20) ) )
   print *, b2%i

   b1 => b2

   print *, 10.5+b2+10+base(20)+b1

   associate ( g => b1 + b2 + child(30) + 40 + child(50) )
      print *,g
      associate ( h => g + 40 )
         print *,h
      end associate
   end associate

   nullify ( b1, b2 )

   allocate ( b1, source = child(100) )
   allocate ( b2, source = child ( base(10) + b1 + 100.20 + child(20) ) )

   print *, b1%i, b2%i

end program
