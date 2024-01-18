!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign039.f
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

  integer, target :: arr(20)=(/(i,i=1,20)/)

  integer, pointer :: ptr(:)

  call sub1(arr,ptr)

  if(lbound(ptr, dim=1).ne. 6) error stop 5
  if(ubound(ptr, dim=1).ne. 15) error stop 6
  if(any(shape(ptr).ne.(/10/))) error stop 7
  if(.not.associated(ptr,arr(11:20))) error stop 8


  contains

    subroutine sub1(tar1,ptr1)
      integer, target :: tar1(:)

      integer, pointer ::ptr1(:)

      ptr1(6:)=>tar1(11:20)

      if(lbound(ptr1, dim=1).ne. 6) error stop 1
      if(ubound(ptr1, dim=1).ne. 15) error stop 2
      if(any(shape(ptr1).ne.(/10/))) error stop 3
      if(.not.associated(ptr1,tar1(11:20))) error stop 4

    end subroutine

end

