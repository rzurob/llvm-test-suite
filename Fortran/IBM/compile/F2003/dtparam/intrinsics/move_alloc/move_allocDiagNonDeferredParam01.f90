!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : move_allocDiagNonDeferredParam01.f
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
!*  2. EACH NONDEFERRED PARAMETER OF THE DECLARED TYPE OF TO SHALL HAVE THE SAME VALUE AS THE CORRESPONDING PARAMETER OF THE DECLARED TYPE OF FROM
!234567890123456789012345678901234567890123456789012345678901234567890
module m
    type dtp(k,l)
        integer,kind :: k=4
        integer,len  :: l=2
    end type
end module

program move_allocDiagNonDeferredParam01

  use m
  implicit none

  type(dtp(4,2)),allocatable  :: dtp1
  type(dtp(4,3)),allocatable  :: dtp2
  type(dtp(2,2)),allocatable  :: dtp3

  class(dtp(4,2)),allocatable :: dtp4
  class(dtp(4,3)),allocatable :: dtp5
  class(dtp(3,2)),allocatable :: dtp6


  call move_alloc(from=dtp1,to=dtp2)
  call move_alloc(from=dtp2,to=dtp1)
  call move_alloc(from=dtp3,to=dtp1)
  call move_alloc(from=dtp1,to=dtp3)

  call move_alloc(from=dtp4,to=dtp5)
  call move_alloc(from=dtp5,to=dtp4)
  call move_alloc(from=dtp6,to=dtp4)
  call move_alloc(from=dtp4,to=dtp6)


end program

