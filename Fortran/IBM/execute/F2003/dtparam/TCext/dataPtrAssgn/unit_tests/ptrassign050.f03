! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/unit_tests/ptrassign050.f
! opt variations: -qnol -qnodeferredlp

!****************************************************************
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                :functional testing of bounds-remapping and bounds-spec
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

  type base(n1,k1)    ! (20,4)
    integer, kind :: k1
    integer, len  :: n1
    real(k1)      :: data
  end type

  contains

    subroutine sub1(ptr,tar)
      type(base(:,4)), pointer, intent(in) :: ptr(:,:)
      type(base(*,4)), target :: tar(:)
      type(base(:,4)), pointer :: dtptr
      integer :: num=1

      if(lbound(ptr, dim=1).ne. 10) error stop 1
      if(lbound(ptr, dim=2).ne. 20) error stop 2
      if(ubound(ptr, dim=1).ne. 19) error stop 3
      if(ubound(ptr, dim=2).ne. 29) error stop 4
      if(any(shape(ptr).ne.(/10,10/))) error stop 5

      do i=20,29
        do j=10,19
          dtptr=>ptr(j,i)
          if(.not.associated(dtptr,tar(num))) error stop 6
          num=num+1
        end do
      end do

    end subroutine

    subroutine sub2(ptr,tar)
      type(base(:,4)), pointer, intent(out) :: ptr(:,:)
      type(base(*,4)), target, intent(in) :: tar(:)
      type(base(:,4)), pointer :: dtptr
      integer :: num=1

      ptr(1:10,1:10)=>tar

      if(lbound(ptr, dim=1).ne. 1) error stop 7
      if(lbound(ptr, dim=2).ne. 1) error stop 8
      if(ubound(ptr, dim=1).ne. 10) error stop 9
      if(ubound(ptr, dim=2).ne. 10) error stop 10
      if(any(shape(ptr).ne.(/10,10/))) error stop 11

      do i=1,10
        do j=1,10
          dtptr=>ptr(j,i)
          if(.not.associated(dtptr,tar(num))) error stop 12
          num=num+1
        end do
      end do

    end subroutine

    subroutine sub3(ptr, tar,nexttar)
      type(base(:,4)), pointer, intent(inout) :: ptr(:,:)
      type(base(*,4)), target, intent(in) :: tar(:)
      type(base(*,4)), target :: nexttar(:)
      type(base(:,4)), pointer :: dtptr
      integer :: num=1

      if(lbound(ptr, dim=1).ne. 50) error stop 19
      if(lbound(ptr, dim=2).ne. 100) error stop 20
      if(ubound(ptr, dim=1).ne. 59) error stop 21
      if(ubound(ptr, dim=2).ne. 109) error stop 22
      if(any(shape(ptr).ne.(/10,10/))) error stop 23

      do i=100,109
        do j=50,59
          dtptr=>ptr(j,i)
          if(.not.associated(dtptr,tar(num))) error stop 24
          num=num+1
        end do
      end do
      num=1

      dtptr=>null()
      ptr=>null()
      ptr(25:34,25:34)=>nexttar


      if(lbound(ptr, dim=1).ne. 25) error stop 25
      if(lbound(ptr, dim=2).ne. 25) error stop 26
      if(ubound(ptr, dim=1).ne. 34) error stop 27
      if(ubound(ptr, dim=2).ne. 34) error stop 28
      if(any(shape(ptr).ne.(/10,10/))) error stop 29

      do i=25,34
        do j=25,34
          dtptr=>ptr(j,i)
          if(.not.associated(dtptr,nexttar(num))) error stop 30
          num=num+1
        end do
      end do



    end subroutine



end module


  use m

  type(base(:,4)), pointer :: ptr1(:,:)
  type(base(20,4)), target  :: tar1(100), tar2(100)
  type(base(:,4)), pointer :: dtptr
  integer :: num=1
  integer :: lbound1, ubound1, lbound2, ubound2


  do i=1,100
    tar1(i)%data=real(i)
    tar2(i)%data=real(i)
  end do

  ptr1(10:19,20:29)=>tar1

  call sub1(ptr1, tar1)

  ptr1=>null()

  call sub2(ptr1, tar2)

  if(lbound(ptr1, dim=1).ne. 1) error stop 13
  if(lbound(ptr1, dim=2).ne. 1) error stop 14
  if(ubound(ptr1, dim=1).ne. 10) error stop 15
  if(ubound(ptr1, dim=2).ne. 10) error stop 16
  if(any(shape(ptr1).ne.(/10,10/))) error stop 17

  do i=1,10
    do j=1,10
      dtptr=>ptr1(j,i)
      if(.not.associated(dtptr,tar2(num))) error stop 18
      num=num+1
    end do
  end do

  ptr1=>null()

  lbound1=50
  lbound2=100
  ubound1=59
  ubound2=109

  ptr1(lbound1:ubound1,lbound2:ubound2)=>tar2
  call sub3(ptr1, tar2, tar1)
  num=1
  if(lbound(ptr1, dim=1).ne. 25) error stop 31
  if(lbound(ptr1, dim=2).ne. 25) error stop 32
  if(ubound(ptr1, dim=1).ne. 34) error stop 33
  if(ubound(ptr1, dim=2).ne. 34) error stop 34
  if(any(shape(ptr1).ne.(/10,10/))) error stop 35

  do i=25,34
    do j=25,34
      dtptr=>ptr1(j,i)
      if(.not.associated(dtptr,tar1(num))) error stop 30
      num=num+1
    end do
  end do

end program

