!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 4, 2011
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 916820
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION:
!*  -----------
!*  Fortran 2008 allows procedure calls where:
!*	  1. The dummy argument has the POINTER and INTENT(IN) attributes, and
!*	  2. The actual argument is a nonpointer that has the TARGET attribute
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
  implicit none

  type dt
    integer i
  end type

  type, extends(dt) :: et
    integer j
  end type

  type, extends(dt) :: ft
    integer k
  end type

  integer, parameter :: MAKE_DT = 1
  integer, parameter :: MAKE_ET = 2
  integer, parameter :: MAKE_FT = 3

contains
  subroutine dt_foo(a)
    class(dt), pointer, intent(in) :: a

    select type(item => a)
      type is (dt)
        print *, "dt: ", item%i
      type is (et)
        print *, "et: ", item%i, item%j
      type is (ft)
        print *, "ft: ", item%i, item%k
    end select
  end subroutine

  subroutine any_foo(a)
    class(*), pointer, intent(in) :: a

    select type(item => a)
      type is (dt)
        print *, "dt: ", item%i
      type is (et)
        print *, "et: ", item%i, item%j
      type is (ft)
        print *, "ft: ", item%i, item%k
      type is (integer)
        print *, "integer: ", item
    end select
  end subroutine

  function make_type(i)
    class(dt), allocatable :: make_type
    integer i

    if (i == MAKE_DT) then
      allocate(make_type, source=dt(1))
    else if (i == MAKE_ET) then
      allocate(make_type, source=et(2, 2))
    else if (i == MAKE_FT) then
      allocate(make_type, source=ft(3, 3))
    else
      error stop
    endif
  end function
end module

use m
implicit none

class(dt), allocatable, target :: a
class(*), allocatable, target :: anyType
integer :: i

i = 133

allocate(a, source = make_type(MAKE_DT))
call dt_foo(a)
deallocate(a)

allocate(a, source = make_type(MAKE_ET))
call dt_foo(a)
deallocate(a)

allocate(a, source = make_type(MAKE_FT))
call dt_foo(a)
deallocate(a)

allocate(a, source = make_type(MAKE_DT))
allocate(anyType, source = a)
call any_foo(anyType)
deallocate(anyType)
deallocate(a)

allocate(a, source = make_type(MAKE_ET))
allocate(anyType, source = a)
call any_foo(anyType)
deallocate(anyType)
deallocate(a)

allocate(a, source = make_type(MAKE_FT))
allocate(anyType, source = a)
call any_foo(anyType)
deallocate(anyType)
deallocate(a)

allocate(anyType, source = i)
call any_foo(anyType)
deallocate(anyType)

end

