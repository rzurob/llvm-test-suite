! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Abstract type with IMPLICIT STATEMENT
!*                                        unlimited polymorphic with implicit statement
!*                                        and then allocated as (non-)polymorphic abstract
!*                                        type with non abstract dynamic type
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

   type, abstract :: base(k1)
      integer, kind :: k1
      integer(k1) :: id
   end type

   type, extends(base) :: child(k2)
      integer, kind :: k2
   end type

end module

program implicit007
   use m
   IMPLICIT class(*) (U-Z)
   pointer :: up1

   class(base(4)), allocatable, target :: b1
   class(base(4)), pointer     :: b2

   allocate(b1, source = child(4,4)(4))
   allocate(b2, source = child(4,4)(5))

   up1=> b1

   if (.not. associated(up1, b1) ) error stop 1_4

   allocate(up1, source = b2)

   if ( associated(up1, b2) ) error stop 2_4


end program
