!*********************************************************************
!*  ===================================================================
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
!*  2. FROM AND TO SHALL BE ALLOCATABLE,DIAGNOSE NON-ALLOCATABLE ARGUMENT
!234567890123456789012345678901234567890123456789012345678901234567890
module m
    type dtp(k,l)
        integer,kind :: k=4
        integer,len  :: l=2
        integer(k)   :: i(l)=0
    end type
end module

program move_allocDiagNonAlloc01

  use m
  implicit none

  type(dtp)               :: from1,to1
  type(dtp(4,2)),target   :: from2,to2
  type(dtp(4,2)),pointer  :: from3,to3
  type(dtp(4,:)),pointer  :: from4,to4
  class(dtp(4,2)),pointer :: from5,to5
  class(*),pointer        :: from6,to6


  call move_alloc(from1,to1)
  call move_alloc(from2,to2)
  call move_alloc(from3,to3)
  call move_alloc(from4,to4)
  call move_alloc(from5,to5)
  call move_alloc(from6,to6)

end program

