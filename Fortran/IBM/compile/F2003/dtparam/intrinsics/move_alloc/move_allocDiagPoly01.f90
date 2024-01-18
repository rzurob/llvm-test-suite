!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : move_allocDiagPoly01.f
!*
!*  DATE                       : Sept. 29 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC(FROM,TO)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.82
!*  2. TO: IT SHALL BE POLYMORPHIC IF FROM IS POLYMORPHIC
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(k,l)
      integer,kind :: k
      integer,len  :: l
   end type
end module

program move_allocDiagPoly01

  use m
  implicit none

  class(dtp(4,2)),allocatable :: dtp1
  class(dtp(4,:)),allocatable :: dtp2
  class(*),allocatable        :: dtp3

  type(dtp(4,2)),allocatable  :: dtp5
  type(dtp(4,:)),allocatable  :: dtp6

  call move_alloc(from=dtp1,to=dtp5)
  call move_alloc(from=dtp2,to=dtp6)
  call move_alloc(from=dtp3,to=dtp1)
  call move_alloc(from=dtp3,to=dtp5)
  call move_alloc(from=dtp3,to=dtp6)

end program

