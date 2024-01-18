!****************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign062.f
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


  integer, pointer :: ptr1(:,:)
  integer, allocatable, target :: tar1(:), tar2(:,:)
  integer, pointer :: ptr2
  integer :: num=1
  integer :: fsource(9,10)
  logical :: mask(9,10), l=.true.


  allocate(tar1(90), source=(/(i,i=1,90)/))
  allocate(tar2(11:19,11:20))

  do i=11,20
    do j=11,19
      tar2(j,i)=num
      num=num+1
    end do
  end do
  num=1

  do i=1,10
    do j=1,9
      fsource(j,i)=1
    end do
  end do

  do i=1,10
    do j=1,9
      mask(j,i)=l
      l=.false.
    end do
  end do

  ptr1(11:19,11:20)=>tar1

  if(lbound(ptr1, dim=1).ne. 11) error stop 1
  if(lbound(ptr1, dim=2).ne. 11) error stop 2
  if(ubound(ptr1, dim=1).ne. 19) error stop 3
  if(ubound(ptr1, dim=2).ne. 20) error stop 4
  if(any(shape(ptr1).ne.(/9,10/))) error stop 5

  do i=11,20
    do j=11,19
      ptr2=>ptr1(j,i)
      if(.not.associated(ptr2,tar1(num))) error stop 6
      num=num+1
    end do
  end do

  if(any(cshift(ptr1,2).ne.cshift(tar2,2))) error stop 7
  if(any(eoshift(ptr1,3).ne.eoshift(tar2,3))) error stop 8
  if(any(merge(ptr1,fsource,mask).ne.merge(tar2,fsource,mask))) error stop 9
  if(any(transpose(ptr1).ne.transpose(tar2))) error stop 10
  if(any(pack(ptr1,mask).ne.pack(tar2,mask))) error stop 11
  if(any(reshape(ptr1,(/56/)).ne.reshape(tar2,(/56/)))) error stop 12
  if(any(spread(ptr1,2,2).ne.spread(tar2,2,2))) error stop 13
  if(any(unpack(ptr1(15,:),mask,0).ne.unpack(tar2(15,:),mask,0))) error stop 14





end


