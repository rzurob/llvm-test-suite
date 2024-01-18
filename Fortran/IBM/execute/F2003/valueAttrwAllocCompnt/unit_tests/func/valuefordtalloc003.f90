!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: valuefordtalloc003.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan, 20, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : value attribute for derived types with allocatable components
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : functional testing of value attribute
!*                               for derived types with allocatable
!*                               components
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  type A
    real, allocatable :: x(:)
    integer :: y
    real :: z
  end type

  type(A) :: A1
  integer :: i

  A1%y=5
  A1%z=1.0

  i=func(A1)
  if(allocated(A1%x)) error stop 1

   contains
     function func(A2)
       type(A), value :: A2
       integer :: i, func

       if(allocated(A2%x)) error stop 2
       allocate(A2%x(5))

       do i=1,5
         A2%x(i)=A2%z
       end do

       func=0

     end function

end program