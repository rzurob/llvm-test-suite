!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : scalar character with deferred length with
!*                               ptr assignment statement
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

program deferLenIntrinAssgn001
   use n

   character(kind=1,len=:), allocatable :: c2

   character(10), target :: t1 = "length ten"
   character(30), target :: t2 = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
   character(30), target :: t3 = "yxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"

   c1 = t1
   print *, c1, "|", len(c1)

   c2 = c1
   print *, c2, "|", len(c2)

   c1 = t2
   c2 = t3

   print *, c1, "|", len(c1)
   print *, c2, "|", len(c2)

   if ( c1(1:29) /= c2(2:30) ) error stop 1_4

end program
