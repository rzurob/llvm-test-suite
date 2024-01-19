!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : scalar character with deferred length with
!*                               argument association, dummy arg is not deferred length
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
   character(100), allocatable :: c1
end module

program deferLenArgAsso003
   use n

   allocate ( c1, source = "length ten" )
   print *, c1, "|", len(c1)

   call foo ( c1 )

   contains

      subroutine foo ( c )
         character(*) :: c

         print *, c, "|", len(c)

      end subroutine

end program
