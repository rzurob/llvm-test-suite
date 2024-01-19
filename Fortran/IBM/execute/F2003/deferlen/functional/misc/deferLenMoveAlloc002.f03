!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : array character with deferred length with MOVE_ALLOC
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
   character(:), allocatable :: c1(:), c2(:)
end module

program deferLenMoveAlloc002
   use n

   allocate ( c1(3:6), source = (/ "I   ", "am  ", "the ", "king" /) )

   print *, c1, len(c1), allocated(c1), lbound(c1), ubound(c1), size(c1)
   call MOVE_ALLOC( c1, c2 )

   print *, c2, len(c2), allocated(c2), lbound(c2), ubound(c2), size(c1)
   print *, allocated(c1)

end program
