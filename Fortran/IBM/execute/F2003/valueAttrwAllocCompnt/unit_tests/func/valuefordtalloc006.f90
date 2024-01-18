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
!*  DESCRIPTION                : functional testing of value attribute
!*                               for derived types with allocatable
!*                               components (deep copy of derived type)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  module m
     type A
      integer, allocatable :: x(:)
    end type

    type B
      type(A), allocatable :: A1
      integer :: y
    end type

    type C
      integer :: z
      type(B), allocatable :: B1
    end type
  end module

  subroutine sub(C2)
    use m
    type(C) :: C2
    value :: C2
    integer :: i

    do i=1,5
      if(C2%B1%A1%x(i).ne.i) error stop 1
    end do

    if(C2%z .ne. 1 .or. C2%B1%y .ne. 5) error stop 2

    do i=1,5
      C2%B1%A1%x(i)=0
    end do

    C2%z=0
    C2%B1%y=0

  end subroutine

   use m

  type(C) :: C1
  integer :: i

  interface
    subroutine sub(C2)
    use m
    type(C) :: C2
    value :: C2
    end subroutine
  end interface

  C1%z=1
  allocate(C1%B1)
  C1%B1%y=5
  allocate(C1%B1%A1)
  allocate(C1%B1%A1%x(5))

  do i=1,5
    C1%B1%A1%x(i)=i
  end do

  call sub(C1)

  do i=1,5
      if(C1%B1%A1%x(i).ne.i) error stop 3
  end do

  if(C1%z .ne. 1 .or. C1%B1%y .ne. 5) error stop 4

end program