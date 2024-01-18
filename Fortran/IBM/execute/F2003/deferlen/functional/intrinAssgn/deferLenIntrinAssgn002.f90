!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : scalar character with deferred length with
!*                               intrinsic assignment statement with long characters
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

program deferLenIntrinAssgn002
   use n

   character(kind=1,len=:), allocatable :: c2
   character(100000), target :: c3

   character(:), allocatable, target :: c4


   allocate ( c4, source = "abcdefghijklmnopqrstuvwxyz\nabcdefghijklmnopqrstuvwxyz\nabcdefghijklmnopqrstuvwxyz\nabcdefghijklmnopqrstuvwxyz\nabcdefghijklmnopqrstuvwxyz&
                         &\nabcdefghijklmnopqrstuvwxyz\nabcdefghijklmnopqrstuvwxyz\nabcdefghijklmnopqrstuvwxyz\nabcdefghijklmnopqrstuvwxyz\nabcdefghijklmnopqrstuvwxyz" )

   print *, len(c4)

   c1 = c4

   print *, c1, len(c1)

   c2 = c4(28:54)

   print *, len(c2)
   print *, c2

   do i=1,100000
      c3(i:i) = 'a'
   end do

   c2 = c3

   print *, len(c2)
   if ( c2 /= c3 ) error stop 1_4

   print *, c3(99999:100000)
   if ( c2(99999:100000) /= 'aa' ) error stop 2_4

end program
