!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : scalar character with deferred length with MOVE_ALLOC
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
   character(:), allocatable :: c1, c2
end module

program deferLenMoveAlloc001
   use n

   allocate ( c1, source = "I love FORTRAN2003" )

   print *, c1, len(c1), allocated(c1)
   call MOVE_ALLOC( c1, c2 )

   print *, c2, len(c2), allocated(c2)
   print *, allocated(c1)

end program
