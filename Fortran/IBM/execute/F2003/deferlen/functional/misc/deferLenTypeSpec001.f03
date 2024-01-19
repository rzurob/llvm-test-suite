!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : specify character with deferred length with type-spec in source of allocate stmt
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


program deferLenTypeSpec001

   character(:), allocatable :: c1(:)

   allocate ( c1(1), source = (/ character(10) :: "abc" /) )
   print *, c1, len(c1), size(c1)

   deallocate ( c1 )

   allocate ( c1(2), source = (/ character(5) :: "abcdef", "ABCDEFGHI" /) )

   print *, c1, len(c1), size(c1)

end program
