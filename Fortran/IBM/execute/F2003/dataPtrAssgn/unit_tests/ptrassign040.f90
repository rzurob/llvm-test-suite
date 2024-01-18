!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign040.f
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

  logical :: arr1(20), arr2(10,10)

  do i=1,20
    arr1(i)=.true.
  end do

  do i=1,10
    do j=1,10
      arr2(j,i)=.true.
    end do
  end do

  call sub1(arr1)
  call sub2(arr2)

  contains

    subroutine sub1(tar1)
      logical, target :: tar1(*)

      logical, pointer :: ptr1(:,:), ptr2

      integer :: num=1

      ptr1(5:9,5:8)=>tar1(1:20)

      if(lbound(ptr1, dim=1).ne. 5) error stop 1
      if(lbound(ptr1, dim=2).ne. 5) error stop 2
      if(ubound(ptr1, dim=1).ne. 9) error stop 3
      if(ubound(ptr1, dim=2).ne. 8) error stop 4
      if(any(shape(ptr1).ne.(/5,4/))) error stop 5

      do i=5,8
        do j=5,9
          ptr2=>ptr1(j,i)
          if(.not.associated(ptr2,tar1(num))) error stop 6
          num=num+1
        end do
      end do

    end subroutine

    subroutine sub2(tar2)
      logical, target :: tar2(10,*)

      logical, pointer :: ptr3(:,:), ptr4

      ptr3(5:,5:)=>tar2(1:10,1:10)

      if(lbound(ptr3, dim=1).ne. 5) error stop 7
      if(lbound(ptr3, dim=2).ne. 5) error stop 8
      if(ubound(ptr3, dim=1).ne. 14) error stop 9
      if(ubound(ptr3, dim=2).ne. 14) error stop 10
      if(any(shape(ptr3).ne.(/10,10/))) error stop 11

      do i=5,14
        do j=5,14
          ptr4=>ptr3(j,i)
          if(.not.associated(ptr4,tar2(j-4,i-4))) error stop 12
        end do
      end do
    end subroutine
end

