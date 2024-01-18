!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept. 30 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC(FROM,TO)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.82
!*  2. DERIVED TYPE COMPONENT HAS ALLOCATABLE LOGICAL COMPONENT
!*  3. VERIFY RESULT AFTER CALL MOVE_ALLOC
!*  4. DEFECT 356904
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(k,l1)
      integer,kind :: k
      integer,len  :: l1
      logical(2*k),allocatable :: log(:)
   end type
   type container(l2)
      integer,len :: l2
      type(dtp(2,l2))    :: dtp3
      type(dtp(2,l2+2))  :: dtp4
   end type
end module

program move_allocLogicalComp01

  use m
  implicit none

  integer :: i=0

  type(dtp(2,:)),allocatable       :: dtp1(:)
  type(dtp(2,:)),allocatable       :: dtp2(:)
  type(container(2)),allocatable   :: contain1
  type(container(:)),allocatable   :: contain2

  logical,parameter :: log(3)=[.true., .false.,.true.]

  allocate(dtp1(-2:2),source=dtp(2,3)(log))

  call move_alloc(from=dtp1,to=dtp2)

  if(allocated(dtp1))                                        error stop 10_4
  if(.not. allocated(dtp2))                                  error stop 11_4
  if(dtp2%l1 /= 3)                                           error stop 12_4

  if(lbound(dtp2,1) /= -2)                                   error stop 13_4
  if(ubound(dtp2,1) /= 2)                                    error stop 14_4
  do i=-2,2
     if(any(dtp2(i)%log .neqv. [.true.,.false.,.true.]))     error stop 15_4
     if(dtp2(i)%log%kind /= 4)                               error stop 16_4
  end do

  allocate(contain1)
  contain1%dtp3=dtp(2,2)([.false.,.true.,.false.])
  contain1%dtp4=dtp(2,4)([.true.,.false.,.true.])

  contain2=contain1
  call move_alloc(from=contain1,to=contain2)

  if(.not. allocated(contain2))                               error stop 17_4
  if(allocated(contain1))                                     error stop 18_4
  if(contain2%l2 /= 2)                                        error stop 19_4
  if(contain2%dtp3%l1 /= 2)                                   error stop 20_4
  if(contain2%dtp4%l1 /= 4)                                   error stop 21_4
  if(any(contain2%dtp3%log .neqv. [.false.,.true.,.false.]))  error stop 22_4
  if(any(contain2%dtp4%log .neqv. [.true.,.false.,.true.]))   error stop 23_4

  call move_alloc(from=contain2%dtp3%log,to=contain2%dtp4%log)

  if(allocated(contain2%dtp3%log))                            error stop 24_4
  if(.not. allocated(contain2%dtp4%log))                      error stop 25_4
  if(any(contain2%dtp4%log .neqv. [.false.,.true.,.false.]))  error stop 26_4

end program

