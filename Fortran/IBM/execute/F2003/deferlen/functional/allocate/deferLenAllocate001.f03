!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : scalar character with deferred length with
!*                               allocate statement
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

program deferLenAllocate001
   use n

   character(kind=1,len=:), allocatable :: c2

   allocate ( c1, source = "length ten" )
   print *, c1, "|", len(c1)

   allocate ( c2, source = c1 )
   print *, c2, "|", len(c2)

   if ( c1 /= c2 ) error stop 1_4

   deallocate ( c1, c2 )

   allocate ( c1, source ="xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" )
   allocate ( c2, source ="xxxxxxxxxxxxxxxxxxxxxxxxxxxxxy" )

   print *, c1, "|", len(c1)
   print *, c2, "|", len(c2)

   if ( c1 == c2 ) error stop 2_4
   if ( c1(1:29) /= c2(1:29) ) error stop 3_4

end program