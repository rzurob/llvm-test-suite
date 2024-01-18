!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : character with deferred length with user defined operator
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
         character(:), allocatable, intent(in) :: a,b
         allocatable :: concat
      end function
   end interface

end module

character(:) function concat(a,b)
   character(:), intent(in), allocatable :: a,b
   allocatable :: concat

   if ( ( .not. allocated (a) ) .or. ( .not. allocated (b) ) ) then
      error stop 1_4
   end if

   allocate ( concat, source = a // b )

end function

program deferLenArgAsso019
   use m

   character(:), allocatable :: c1, c2

   allocate ( c1, source = "I love IBM " )
   allocate ( c2, source = "FORTRAN 2003!! " )

   print *, c1 + c2
   print *, c1 + c2 + c2 + c1

end program
