!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : scalar character with deferred length as structure component with
!*                               derived type intrinsic assignment
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
      character(:), allocatable :: c
   end type

end module

program deferLenIntrinAssgn004
   use n

   type base1
      character(kind=1,len=:), pointer :: c
   end type

   type(base)  :: b1
   type(base1) :: b2

   character(0), target :: c0
   character(10), target :: c10 = "abcdefghij"
   character(50), target :: c50 = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

   b1 = base( c0 )
   b2 = base1( c10 ) ! 10 characters

   print *, b1%c, len(b1%c), allocated(b1%c)
   print *, b2%c, len(b2%c), associated(b2%c, c10)

   b1 = base( c50 ) ! 50 characters
   b2 = base1( c10(4:5) ) ! 2 characters

   print *, b1%c, len(b1%c), allocated(b1%c)
   print *, b2%c, len(b2%c), associated(b2%c, c10)

end program
