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
!*  DESCRIPTION                : Operator: operators with array sections
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
      integer :: i
      contains
         procedure :: combine
         generic :: operator(+) => combine
   end type

   contains

      type(base) function combine ( a, b )
         allocatable :: combine(:)
         class(base), intent(in) :: a
         class(base), intent(in) :: b(:)

         allocate ( combine(size(b)), source = (/ ( base (  b(i)%i + a%i ) , i = 1, size(b)) /) )

      end function

end module

program genericOperatorArray007
   use m

   type(base) :: b1(5), b2(:), b3(:)
   pointer  b2, b3
   

   b1 = (/ base(1), base(2), base(3), base(4), base(5) /)
   allocate ( b2(5), source = (/ base(21), base(22), base(23), base(24), base(25) /) )
   b3 => b2

   b3(1:5:2) = b1(1) + b2(1:5:2)
   print *, b3%i, b2%i

   nullify ( b2, b3 )

   allocate ( b2(3), b3(4) )

   b2 = (/ base(1), base(2), base(3) /)

   b3 = b1(5) + b2( (/ b1(1:3)%i, b1(1)%i /) )
   print *, b3

   b2 = b3(2) + (/(b3(2), i=1,3)/)
   print *, b2

   b2(2:3) = b2(1) + b3(4:1:-2)

   print *, b2

end program
