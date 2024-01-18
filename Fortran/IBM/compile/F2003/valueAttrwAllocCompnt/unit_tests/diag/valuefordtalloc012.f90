!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan, 20, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : value attribute for derived types with allocatable components
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : diagnostic testing of %VAL with
!*                               derived types with allocatable
!*                               components
!*
!234567890123456789012345678901234567890123456789012345678901234567890
  module m
    type dt1
      integer :: i
      real, allocatable :: r
    end type
  end module

  program mainf
    use m

    type(dt1) :: temp
    integer :: i

    temp%i=5
    allocate(temp%r)
    temp%r=5.0

    call sub(%VAL(temp))
    i=func(%VAL(temp))

    contains
      subroutine sub(x)
        type(dt1) :: x

        x%i=0
        x%r=0.0

      end subroutine

      function func(x)
        type(dt1) :: x
        integer :: func

        x%i=0
        x%r=0.0

        func=0
      end function

end program






