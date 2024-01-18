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
!*  DESCRIPTION                : Operator: operators with zero sized array
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
         pointer :: combine(:)
         class(base), intent(in) :: a
         class(base), intent(in) :: b(:)

         allocate ( combine(size(b)), source = (/ ( base (  b(i)%i + a%i ) , i = 1, size(b)) /) )
         print *, 'inside combine:', size(combine)

      end function

end module

program genericOperatorArray008
   use m

   type(base) :: b1, b2(:), b3(:)
   pointer :: b2, b3

   b1 = base(1)
   allocate ( b2(0) )
   b3 => b2

   b3 = b1 + b2

   nullify ( b2, b3)
   allocate ( b2(0) )

   b3 => b1 + b2
   print *, 'main:', size ( b3 )

end program
