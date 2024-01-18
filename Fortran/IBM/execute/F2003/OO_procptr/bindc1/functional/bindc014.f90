!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 06/07/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Procedure Pointer with BIND(C) feature
!*                                        Procedure pointer and (non-bind(c) type)Procedure pointer component pointing at C recursive function
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

   use ISO_C_BINDING

   interface
      integer(C_INT) recursive function getfibonacci(n) bind(C)
         import C_INT
         integer(C_INT), intent(in), value :: n
      end function
   end interface

   type, bind(c) :: fibonacci
      integer(C_INT) :: i, n
   end type

   type :: base
      type(fibonacci) :: f
      procedure(getfibonacci), nopass, pointer :: pp
   end type

end module

   use m

   type(base) :: b1
   procedure( getfibonacci ), pointer :: pp1
   integer(C_INT) :: i1

   b1 = base( fibonacci( 0, 0 ), getfibonacci )

   b1%f%i = b1%pp(b1%f%n)
   if ( b1%f%i /= 0 ) error stop 1_4

   b1 = base( fibonacci( 0, 1 ), getfibonacci )
   b1%f%i = b1%pp(b1%f%n)
   if ( b1%f%i /= 1 ) error stop 2_4

   b1 = base( fibonacci( 0, 30 ), getfibonacci )
   b1%f%i = b1%pp(b1%f%n)
   if ( b1%f%i /= 832040 ) error stop 3_4

   pp1 => b1%pp

   i1 = pp1( 35 )
   if ( i1 /= 9227465 ) error stop 4_4

end
