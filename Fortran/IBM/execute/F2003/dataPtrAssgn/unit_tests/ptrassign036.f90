!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign036.f
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

  real :: arr(25)=(/(real(i), i=1,25)/)

  call sub1(arr)

  contains

    subroutine sub1(tar1)
      real, target :: tar1(:)

      real, pointer :: ptr1(:,:), ptr2

      integer :: num=1

      ptr1(10:14,15:19)=>tar1

      if(lbound(ptr1, dim=1).ne. 10) error stop 1
      if(lbound(ptr1, dim=2).ne. 15) error stop 2
      if(ubound(ptr1, dim=1).ne. 14) error stop 3
      if(ubound(ptr1, dim=2).ne. 19) error stop 4
      if(any(shape(ptr1).ne.(/5,5/))) error stop 5

      do i=15,19
        do j=10,14
          ptr2=>ptr1(j,i)
          if(.not.associated(ptr2,tar1(num))) error stop 6
          num=num+1
        end do
      end do
    end subroutine

end

