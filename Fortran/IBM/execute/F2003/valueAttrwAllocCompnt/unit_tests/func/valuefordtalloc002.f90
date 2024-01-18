!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: valuefordtalloc002.f
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
  allocate(A1%x(A1%y))

  do i=1,5
    A1%x(i)=A1%z
  end do

  call sub(A1)
  do i=1,5
    if(A1%x(i) .ne. A1%z) error stop 1
  end do

   contains
     subroutine sub(A2)
       type(A) :: A2
       integer :: i
       value :: A2

       do i=1,5
         if(A2%x(i) .ne. A2%z) error stop 2
       end do

       do i=1,5
         A2%x(i)=0.0
       end do

     end subroutine

end program