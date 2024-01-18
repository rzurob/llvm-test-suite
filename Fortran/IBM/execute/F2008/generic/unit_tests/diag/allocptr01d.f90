! Interp F08/0001 (part of corrigendum 1) says that intent(in) pointer dummy
! arguments are not distinguishable from allocatable dummy arguments.

program main
  implicit none
  interface foo
    integer function func2(x)
      integer, allocatable, intent(in) :: x
    end function

    integer function func1(x)
      integer, pointer, intent(in) :: x
    end function
  end interface

  integer result

  integer, target :: t
  integer, allocatable, target :: at

  ! t is not allocatable or pointer, has the target attribute,
  ! and the corresponding dummy argument in foo has the intent(in)
  ! attribute.  In func1, the corresponding dummy argument has the
  ! pointer attribute and so auto targetting can occur.
  t = 4
  result = foo(t)

  ! at is not allocatable or pointer, has the target attribute,
  ! and the corresponding dummy argument in foo has the intent(in)
  ! attribute.  In func1, the corresponding dummy argument has the
  ! pointer attribute and so auto targetting can occur.  But in
  ! func2, the corresponding dummy argument is allocatable.  So
  ! this is ambiguous.
  at = 5
  result = foo(at)

  ! This is a diagnostic test case.  If we get here, fail
  error stop 1
end program
