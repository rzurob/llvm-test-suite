!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: valuefordtalloc005.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan, 20, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : value attribute for derived types with allocatable components
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : functional testing of value attribute
!*                               for derived types with allocatable
!*                               components (deep copy of derived type)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  module m
    type A
      real  :: x
      integer :: y
    end type

    type B
      type(A), allocatable :: A1
      integer :: z
    end type

    contains

      function func(B1)
        type(B), value :: B1
        type(B) :: func

        if(B1%A1%x/=1.0 .or. B1%A1%y/=1 &
       &   .or. B1%z/=2.0) error stop 1

       func=B1

       B1%z=0.0
       B1%A1%x=0.0
       B1%A1%y=0

      end function

  end module

  use m

  type(B) :: B2, B3

  B2%z=2.0
  allocate(B2%A1)
  B2%A1%x=1.0
  B2%A1%y=1

  B3=func(B2)

  if((B2%z .ne. B3%z) .or. (B2%A1%x .ne. B3%A1%x) &
  &   .or. (B2%A1%y .ne. B3%A1%y)) error stop 2

end program

