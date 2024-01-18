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
!*  DESCRIPTION                : Operator: with elemental function
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
      integer :: x = -999
      contains
         generic :: operator(*) => mul
         procedure :: mul
   end type

   contains

      elemental type(base) function mul ( a, b )
         class(base), intent(in) :: a,b

         mul%x = a%x * b%x

      end function

end module

program genericOperatorElemental001
   use m

   type(base) :: b1, b11(3)
   type(base), pointer     :: b2(:), b3(:)

   b1 = base(5)
   b11 = (/ base(1), base(2), base(3) /)

   b11 = b11 * b1
   print *, b11%x

   b11 = b11 * b11
   print *, b11%x

   allocate ( b2(5), source = (/ ( base(i), i=-2,2 ) /) )
   allocate ( b3(5), source = (/ ( base(i), i=2,-2,-1 ) /) )

   b11 = b2(1:5:2) * b3((/1,3,5/))
   print *, b11%x
   
   b11 = b3((/1,2,5/)) * b2(5)
   print *, b11%x

end program
