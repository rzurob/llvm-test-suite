!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: 12.4.5 Resolving type-bound procedure references
!*                                         vi ) contains both assumed-size array of different ranks
!*
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
         procedure :: add1
         generic :: operator (+) => add1
   end type

   contains

   type(base) function add1 (a,b)
      class(base), intent(in) :: a, b(*)

      add1%i = a%i

      do j = 1, 4
         add1%i = add1%i + b(j)%i
      end do

      print *, 'add1'

   end function

end module

program genericOperatorResolve006a
   use m

   type(base) :: b
   type(base) :: b1(4), b2(4), b3(2,2), b4(2,2)

   interface operator(+)
      type(base) function add2 (a,b)
         import base
         class(base), intent(in) :: a, b(2,*)
      end function
   end interface

   b1 = (/ ( base(j), j = 1, 4 ) /)
   b2 = (/ ( base(j+4), j = 1, 4 ) /)

   b3 = reshape ( source = b1, shape = (/2,2/) )
   b4 = reshape ( source = b2, shape = (/2,2/) )

   b = base(10) + b1
   print *, b%i

   b = base(10) + b2
   print *, b%i

   b = base(10) + b3
   print *, b%i

   b = base(10) + b4
   print *, b%i

end program

type(base) function add2 (a,b)
   use m, only: base
   class(base), intent(in) :: a, b(2,*)

   add2%i = a%i

   do j = 1, 2
      do k = 1, 2
         add2%i = add2%i + b(j,k)%i
      end do
   end do

   print *, 'add2'

end function
