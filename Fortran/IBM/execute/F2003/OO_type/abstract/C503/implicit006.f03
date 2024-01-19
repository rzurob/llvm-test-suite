! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Abstract type with IMPLICIT STATEMENT
!*                                        IMPLICIT legal polymorphic non-abstract type that is extension of abstract type
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type, abstract :: base
      integer :: id
   end type

   type, extends(base) :: child
   end type

end module

program implicit006
   use m
   IMPLICIT class(child) (A-F)

   pointer Aa
   allocatable Bb

   type(child) :: c1 = child(5)
   class(child), allocatable :: c2

   allocate(c2, source = child(5) )

   allocate(Aa, source = c1)
   allocate(Bb, source = c2)

   if ( Aa%id .ne. c1%id ) error stop 1_4
   if ( Bb%id .ne. c1%id ) error stop 2_4

end program