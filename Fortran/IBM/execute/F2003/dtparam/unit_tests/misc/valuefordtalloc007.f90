!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: valuefordtalloc007.f
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
!*                               components (derived types parameters, very simple)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  module m
    type A (l,k)
      integer, len  :: l
      integer, kind  :: k

      real(k) :: x
      real(k) , allocatable :: y(:)
    end type

  end module

  use m

  type(A(5,4)) :: A1
  integer :: i

  A1%x=1.0
  allocate(A1%y(5))

  do i=1,5
    A1%y(i)=i
  end do

  call sub(A1)

  do i=1,5
    if(A1%y(i).ne.i) error stop 1
  end do

  if((A1%x .ne. 1.0) .or. (A1%l .ne. 5) .or. (A1%k .ne. 4)) error stop 2

    contains

    subroutine sub(A2)

      type(A(5,4)), value :: A2
      integer :: j

      do j=1,5
        if(A2%y(j).ne.j) error stop 3
      end do

      if((A2%x .ne. 1.0) .or. (A2%l .ne. 5) .or. (A2%k .ne. 4)) error stop 4

      A2%x=0.0

      do j=1,5
        A2%y(j)=0.0
      end do

    end subroutine

end program




