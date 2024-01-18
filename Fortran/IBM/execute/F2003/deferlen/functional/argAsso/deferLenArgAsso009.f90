!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : derived type containing array character with deferred length with
!*                               argument association with pointer/target
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

module n
   type base
      character(:), pointer :: c(:)
   end type

   type(base) :: b1, b2

end module

program deferLenArgAsso009
   use n

   character(1), target :: c(26) = (/ ( char(i), i=65,90 ) /)

   b1 = base( null() )

   b1%c => c

   b2 = b1

   print *, b1%c, len(b1%c)
   print *, b2%c, len(b2%c)

   call foo ( b1 )

   print *, b1%c, len(b1%c)
   print *, b2%c, len(b2%c)

   contains

      subroutine foo ( c )
         type(base) :: c
         print *, c%c, len(c%c)

         c%c = 'x'

      end subroutine

end program