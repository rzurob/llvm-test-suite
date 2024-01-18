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
         character(*), intent(in) :: a,b
         allocatable :: concat
      end function
   end interface

end module

character(:) function concat(a,b)
   character(*), intent(in) :: a,b
   allocatable :: concat

   allocate ( concat, source = a // b )

end function

program deferLenArgAsso018
   use m

   character(:), allocatable :: c1, c2

   allocate ( c1, source = "WHAT IS " )
   allocate ( c2, source = "FORTRAN?" )

   print *, c1 + c2
   print *, "question: " + c1 + c2 + " answer: " + "I don't know..."

end program
