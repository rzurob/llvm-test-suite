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
!+  DESCRIPTION                : Binary Operator: Arrays and with pass attribute
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
         procedure, pass(b) ::  base_basearray_base
         procedure, pass ::  base_base_basearray
         procedure, pass :: base_base_base
         generic :: operator(+) => base_base_basearray, base_basearray_base, base_base_base
   end type

   contains

   type(base) function base_basearray_base ( a, b )
      class(base), intent(in) :: a(:)
      class(base), intent(in) :: b

      base_basearray_base = b

      do i = 1, size(a)
         base_basearray_base%i = a(i)%i + base_basearray_base%i
      end do

   end function

   type(base) function base_base_basearray ( a, b )
      class(base), intent(in) :: a
      class(base), intent(in) :: b(:)

      base_base_basearray = a

      do i = 1, size(b)
         base_base_basearray%i = base_base_basearray%i + b(i)%i
      end do

   end function

   type(base) function base_base_base ( a, b )
      class(base), intent(in) :: a
      class(base), intent(in) :: b

      base_base_base%i = a%i + b%i

   end function

end module

program genericOperatorPass012
   use m

   type(base) :: b1, b2, b3(4), b4(5)

   b1 = base(10) + base(20) + (/ base(1), base(2), base(3), base(4) /)

   b2 = b1 + (/ base(1), base(2), base(3), base(4) /) + (/ base(1), base(2), base(3), base(4) /) + (/ base(1), base(2), base(3), base(4) /)

   b3 = (/ base(1), base(2), base(3), base(4) /)

   b4 = b3 + b1 + b2 + b3 + b1 + base(5)

   print *,b1
   print *,b2
   print *,b3
   print *,b4

end program
