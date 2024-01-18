!****************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign060.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 31, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                :functional testing of bounds-remapping and bounds-spec
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  real, pointer :: ptr(:,:), ptr2
  real, target :: tar(100)
  integer :: num=1

  real :: result(10,10)

  real :: data=1.0


  do i=1,100
    tar(i)=cos(data)
    data=data+1.0
  end do
  data=1.0


  do i=1,10
    do j=1,10
      result(j,i)=cos(data)
      data=data+1.0
    end do
  end do

  ptr(101:int(sqrt(size(tar)*1.0))+(101-1),111:int(sqrt(size(tar)*1.0))+(111-1))=>tar

  if(lbound(ptr, dim=1).ne. 101) error stop 1
  if(lbound(ptr, dim=2).ne. 111) error stop 2
  if(ubound(ptr, dim=1).ne. 110) error stop 3
  if(ubound(ptr, dim=2).ne. 120) error stop 4
  if(any(shape(ptr).ne.(/10,10/))) error stop 5

  do i=111,120
    do j=101,110
      ptr2=>ptr(j,i)
      if(.not.associated(ptr2,tar(num))) error stop 6
      num=num+1
    end do
  end do

  if(maxval(ptr).ne.maxval(tar)) error stop 7
  if(minval(ptr).ne.minval(tar)) error stop 8
  if(any(maxloc(ptr).ne.maxloc(result))) error stop 9
  if(any(minloc(ptr).ne.minloc(result))) error stop 10
  if(size(ptr).ne. size(tar)) error stop 11
  if(product(ptr).ne.product(tar)) error stop 12
  if(sum(ptr).ne.sum(tar)) error stop 13


end
