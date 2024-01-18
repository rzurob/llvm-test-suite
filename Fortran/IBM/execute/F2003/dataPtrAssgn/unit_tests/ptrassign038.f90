!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign038.f
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

  integer, target :: arr(20)=(/(i,i=1,20)/), num1=1

  integer, pointer :: ptr(:,:), ptr3

  call sub1(arr,ptr)

  if(lbound(ptr, dim=1).ne. 1) error stop 7
  if(lbound(ptr, dim=2).ne. 1) error stop 8
  if(ubound(ptr, dim=1).ne. 5) error stop 9
  if(ubound(ptr, dim=2).ne. 4) error stop 10
  if(any(shape(ptr).ne.(/5,4/))) error stop 11

  do i=1,4
    do j=1,5
      ptr3=>ptr(j,i)
      if(.not.associated(ptr3,arr(num1))) error stop 12
      num1=num1+1
    end do
   end do

  contains

    subroutine sub1(tar1,ptr1)
      integer, target :: tar1(:)

      integer :: num=1

      integer, pointer ::ptr1(:,:), ptr2

      ptr1(1:5,1:4)=>tar1

      if(lbound(ptr1, dim=1).ne. 1) error stop 1
      if(lbound(ptr1, dim=2).ne. 1) error stop 2
      if(ubound(ptr1, dim=1).ne. 5) error stop 3
      if(ubound(ptr1, dim=2).ne. 4) error stop 4
      if(any(shape(ptr1).ne.(/5,4/))) error stop 5

      do i=1,4
        do j=1,5
          ptr2=>ptr1(j,i)
          if(.not.associated(ptr2,tar1(num))) error stop 6
          num=num+1
        end do
      end do

    end subroutine

end

