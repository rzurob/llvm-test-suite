!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 05/01/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : array character with deferred length with user defined operator
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

   interface operator(+)
      character(:) function concat(a,b)
         character(*), intent(in) :: a(:),b(:)
         allocatable :: concat(:)
      end function
   end interface

end module

character(:) function concat(a,b)
   character(*), intent(in) :: a(:),b(:)
   allocatable :: concat(:)

   allocate ( character(len(a)+len(b)) :: concat ( size(a) ) )

   do i = 1, size(a)
      concat(i) = a(i) //b(i)
   end do

end function

program deferLenArgAsso021
   use m

   character(:), allocatable :: c1(:), c2(:)

   character(:), allocatable :: c3(:)

   allocate ( c1(3), source = (/ 'abc', 'def', 'ghi' /) )
   allocate ( c2(3), source = (/ 'ABC', 'DEF', 'GHI' /) )

   allocate ( c3(3), source = c1 + c2 )

   print *, c1, len(c1)
   print *, c2, len(c2)
   print *, c3, len(c3)

end program
