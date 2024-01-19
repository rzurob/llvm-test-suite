!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: operators with interface block with zero sized array
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

   interface operator(+)
      module procedure atoacombine
   end interface

   contains

      type(base) function combine ( a, b )
         pointer :: combine(:)
         class(base), intent(in) :: a
         class(base), intent(in) :: b(:)

         allocate ( combine(size(b)), source = (/ ( base (  b(i)%i + a%i ) , i = 1, size(b)) /) )
         print *, 'stoa'
      end function

      type(base) function atoacombine ( a, b )
         pointer :: atoacombine(:)
         class(base), intent(in) :: a(:)
         class(base), intent(in) :: b(:)

         allocate ( atoacombine(size(b)), source = (/ ( base (  b(i)%i + nth(a,i) ) , i = 1, size(b)) /) )
         print *, 'atoa'
      end function

      integer function nth(array, index)
        type (base) :: array(:)
        integer :: index
        nth = 0
        if (lbound(array,1) <= index .and. index <= ubound(array,1)) nth = array(index)%i
      end function nth

end module

program genericOperatorArray010
   use m

   type(base) :: b1(4), b2(:), b3(:)
   pointer :: b2, b3

   b1 = (/ base(1), base(2), base(3), base(4) /)
   allocate ( b2(4), b3(4) )

   b2 = (/ base(1), base(2), base(3), base(4) /)
   b3 = (/ base(1), base(2), base(3), base(4) /)

   b1 = b1(1) + b2 + b3(4:1) + b1
   print *, b1%i

   b2 = b2(1) + b2(1:4:-1) + ( b2(2) + b3(1:4:-1) ) + b2
   print *, b2%i

   b3 = b3(1) + b3( (/1,4,2,3/) ) + ( b3(2) + b3( (/4,4,4,1/) ) + b3(4:) + b3(4:1) ) + b3
   print *, b3%i

end program
