!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign010.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 31, 2010
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  : -qxlf2003=polymorphic
!*
!*  DESCRIPTION                :C716 - diagnostic
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  type dt1
    integer :: num1
  end type

  type, extends(dt1) :: dt1ext
  end type

  type dt2
    complex :: num2
  end type

  type, extends(dt2) :: dt2ext
  end type



  class(dt1), pointer :: ptr1(:)
  type(dt1), target :: arr1(1:10)
  class(dt1ext), pointer :: ptr2(:)

  class(dt2), pointer :: ptr3(:)
  type(dt2), target :: arr2(1:10)
  class(dt2ext), pointer :: ptr4(:)

  ptr1(5:)=>arr2
  ptr2(10:19)=>arr1
  ptr2(10:10)=>arr2

  ptr3(4:)=>arr1
  ptr4(5:14)=>arr2
  ptr4(5:14)=>arr1

end

