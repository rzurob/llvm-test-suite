!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : derived type containing array character with deferred length with
!*                               argument association
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
      character(:), allocatable :: c(:)
   end type

   type(base) :: b1

end module

program deferLenArgAsso008
   use n

   b1 = base( (/ ( char(i), i=65,90 ) /) )

   call foo ( b1 )

   print *, b1%c, len(b1%c)

   contains

      subroutine foo ( c )
         type(base) :: c
         print *, c%c, len(c%c)

      end subroutine

end program