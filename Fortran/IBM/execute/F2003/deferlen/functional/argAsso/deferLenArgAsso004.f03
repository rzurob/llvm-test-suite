!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : scalar character with deferred length with
!*                               argument association, actual not associated
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
   character(:), allocatable :: c1
end module

program deferLenArgAsso004
   use n

   call foo ( c1 )

   print *, "outside: ", c1, "|", len(c1)

   contains

      subroutine foo ( c )
         character(:), allocatable :: c

         allocate ( c, source = "abcdefghi" )
         print *, "inside: ", c, "|", len(c)

      end subroutine

end program