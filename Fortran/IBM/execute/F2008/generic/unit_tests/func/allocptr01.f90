! Test Fortran 2008 generic rule about pointer and allocatable dummy arguments
! being distinguishable.

module constants
  integer, parameter :: from_func1 = 1
  integer, parameter :: from_func2 = 2
end module

program main
  use constants
  implicit none
  interface foo
    integer function func2(x)
      integer, allocatable :: x
    end function

    integer function func1(x)
      integer, pointer :: x
    end function
  end interface

  integer result

  integer, target :: t
  integer, allocatable :: a
  integer, pointer :: p

  ! a is allocatable.  We should resolve to func2
  allocate(a, source=6)
  result = foo(a)
  if (result /= from_func2) then
    print *, result
    error stop 3
  end if

  ! p is a pointer.  We should resolve to func1
  t = 4
  p => t
  result = foo(p)
  if (result /= from_func1) then
    print *, result
    error stop 4
  end if
end program

integer function func1(x)
  use constants
  integer, pointer :: x
  print *, 'func1.  x=', x
  func1 = from_func1
end function

integer function func2(x)
  use constants
  integer, allocatable :: x
  print *, 'func2.  x=', x
  func2 = from_func2
end function
