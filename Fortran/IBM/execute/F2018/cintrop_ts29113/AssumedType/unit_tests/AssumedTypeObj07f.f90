!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Apr 14, 2012
!*                             : IBM Software Solutions China Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop Assumed-type object
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Calling a BIND(C) procedure from Fortran
!*                               where the procedure is defined in C with
!*                               assumed-type object.
!*                               - Actual arg is of polymorphic object including
!*                                 unlimited polymorphic
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
  use, intrinsic :: iso_c_binding
  implicit none

  type dt
    integer(c_int) i
  end type

  type, extends(dt) :: et
    integer(c_int) j
  end type

  type, extends(dt) :: ft
    integer(c_int) k
  end type

  integer, parameter :: MAKE_DT = 1
  integer, parameter :: MAKE_ET = 2
  integer, parameter :: MAKE_FT = 3

  contains
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

program AssumedTypeObj07f
  use, intrinsic :: iso_c_binding
  use m
  implicit none

  type new_t
    integer(c_int) :: newV = -1
  end type

  class(*), allocatable :: a
  integer :: i = 77
  type(new_t) :: n

  interface
    subroutine c_func_dyn(a, flag) BIND(c)
      use, intrinsic :: iso_c_binding
      implicit none

      TYPE(*) :: a, flag
    end subroutine c_func_dyn
  end interface

  allocate(a, source=make_type(MAKE_DT))
  call sub(a, 1)
  deallocate(a)

  allocate(a, source=make_type(MAKE_ET))
  call sub(a, 2)
  deallocate(a)

  allocate(a, source=make_type(MAKE_FT))
  call sub(a, 3)
  deallocate(a)

  allocate(a, source=i)
  call sub(a, 4)
  deallocate(a)

  allocate(a, source=n)
  call sub(a, 5)
  deallocate(a)

  contains
    subroutine sub(a, flag)
      TYPE(*) :: a
      TYPE(*) :: flag

      call c_func_dyn(a, flag)
    end
end

