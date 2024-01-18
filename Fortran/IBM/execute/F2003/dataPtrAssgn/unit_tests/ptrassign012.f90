!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign012.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 31, 2006s
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  : -qxlf2003=polymorphic
!*
!*  DESCRIPTION                :C717 functional
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  type dt1
    real :: num1
  end type

  type dt2
    sequence
    integer :: num2
  end type


  class(*), pointer :: ptr1(:)
  class(*), target, allocatable :: arr1(:)
  class(*), pointer :: ptr2(:)
  type(dt2), pointer :: ptr3(:)

  allocate(arr1(5), source=(/1,1,1,1,1/))

  ptr1(5:)=>arr1
  if(.not.associated(ptr1,arr1)) error stop 1
  ptr2(6:)=>ptr1
  if(.not.associated(ptr2,arr1)) error stop 2
  ptr3(7:11)=>ptr1
  if(.not.associated(ptr3,arr1)) error stop 3

end
