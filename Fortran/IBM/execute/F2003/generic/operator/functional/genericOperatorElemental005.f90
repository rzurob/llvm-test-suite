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
!*  DESCRIPTION                : Operator: with elemental function with abstract types
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

   type, abstract :: base
      integer :: x = -999
      contains
         generic :: operator(*) => mul
         procedure(inte), deferred :: mul
   end type

   type, extends(base) :: c1
      contains
         procedure :: mul => mc1
   end type

   interface
      type(c1) elemental function inte (a, b)
         import base, c1
         class(base), intent(in) :: a, b
      end function
   end interface

   contains

      type(c1) elemental function mc1 (a, b)
         class(c1), intent(in) :: a
         class(base), intent(in) :: b

         mc1%x = a%x * b%x

      end function

end module

module n
   use m

   type, extends(c1) :: c2
   end type

end module

program genericOperatorElemental005
   use n

   class(base), allocatable :: b1
   type(c1) :: c11
   class(c1), allocatable :: c12
   
   type(c2) :: c21
   class(c2), allocatable :: c22
   
   allocate ( b1, source = c1(10) * c1(20) ) 
   c11 = c1(2) * c2(20)
   
   allocate (c12, source = c11 * c2(20) )
   
   print *, b1%x
   print *, c11%x
   print *, c12%x
   
   c21 = c2(100)
   allocate ( c22, source = c2(50))

   c11 = c21 * c22
   print *, c11%x
   deallocate ( c12 )
   allocate ( c12, source = c21 * c2(5) * c22 )
   print *, c12%x


end program
