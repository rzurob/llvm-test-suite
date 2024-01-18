!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign041.f
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

      logical, pointer :: ptr1(:)

      ptr1(11:)=>tar1(1:20)

      if(lbound(ptr1, dim=1).ne. 11) error stop 1
      if(ubound(ptr1, dim=1).ne. 30) error stop 2
      if(any(shape(ptr1).ne.(/20/))) error stop 3
      if(.not.associated(ptr1,tar1(1:20))) error stop 4

    end subroutine

    subroutine sub2(tar2)
      logical, target :: tar2(10,*)

      logical, pointer :: ptr2(:,:)

      ptr2(-11:,11:)=>tar2(1:10,1:10)

      if(lbound(ptr2, dim=1).ne. -11) error stop 5
      if(lbound(ptr2, dim=2).ne. 11) error stop 6
      if(ubound(ptr2, dim=1).ne. -2) error stop 7
      if(ubound(ptr2, dim=2).ne. 20) error stop 8
      if(any(shape(ptr2).ne.(/10,10/))) error stop 9
      if(.not.associated(ptr2,tar2(1:10,1:10))) error stop 10

    end subroutine
end

