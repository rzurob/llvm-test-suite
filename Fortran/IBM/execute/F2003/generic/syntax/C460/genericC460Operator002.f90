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
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : C460: specific-binding exists in parent type
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

   type base
      integer i
      contains
         procedure, pass :: add
   end type

   type, extends(base) :: child
      contains
         generic :: operator(+) => add
   end type

   interface
      class(base) function add(a, b)
         import base
         class(base), intent(in) :: a, b
         allocatable :: add
      end function
   end interface

end module

program genericC460Operator002
   use m

   type(base) :: c1

   c1 = child(123) + base(123)
   if ( c1%i /= 246 ) error stop 1_4

   c1 = c1 + child (123)
   if ( c1%i /= 369 ) error stop 2_4

end program

class(base) function add(a, b)
   use m, only: base
   class(base), intent(in) :: a, b
   allocatable :: add

   allocate ( add, source = a )

   add%i = add%i + b%i

end function

