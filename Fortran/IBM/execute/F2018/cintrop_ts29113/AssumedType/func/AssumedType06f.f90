!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 13, 2012
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop: Assumed Type objects
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : BIND(C) procedures defined in Fortran,
!*                               call in Fortran
!*                               dummy argument is scalar assumed type
!*                               actual argument is of derived type
!*                               - basic
!*                               - extended
!*                               - with allocatable component
!*                               - with pointer component
!*                               - with sequence attribute
!*
!**********************************************************************
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
  implicit none

  type base
    integer :: i
  end type base

  type, extends(base) :: child
    real :: x
  end type child

  type dt1
    integer, allocatable :: arr
  end type dt1

  type dt2
    integer, pointer :: ptr
  end type dt2

  type dt3
    sequence
    integer :: j
  end type dt3
end module mod

program AssumedType06f
use mod
implicit none

interface
   subroutine sub(a) bind(c)
      use, intrinsic :: iso_c_binding
      type(*) :: a
   end subroutine sub

   subroutine sub_optional(a) bind(c)
     use, intrinsic :: iso_c_binding
     type(*), optional :: a
   end subroutine sub_optional

   subroutine sub_target(a) bind(c)
     use, intrinsic :: iso_c_binding
     type(*), target :: a
   end subroutine sub_target

   subroutine sub_volatile(a) bind(c)
     use, intrinsic :: iso_c_binding
     type(*), volatile :: a
   end subroutine sub_volatile
end interface

type(base) :: b
type(child) :: c
type(dt1) :: dt1_0
type(dt2) :: dt2_0
type(dt3) :: dt3_0

call sub(b)
call sub(c)
call sub(dt1_0)
call sub(dt2_0)
call sub(dt3_0)

call sub_optional(b)
call sub_optional(c)
call sub_optional(dt1_0)
call sub_optional(dt2_0)
call sub_optional(dt3_0)

call sub_target(b)
call sub_target(c)
call sub_target(dt1_0)
call sub_target(dt2_0)
call sub_target(dt3_0)

call sub_volatile(b)
call sub_volatile(c)
call sub_volatile(dt1_0)
call sub_volatile(dt2_0)
call sub_volatile(dt3_0)

end program AssumedType06f

subroutine sub(a)
  use, intrinsic :: iso_c_binding
  type(*) :: a
end subroutine sub

subroutine sub_optional(a)
  use, intrinsic :: iso_c_binding
  type(*), optional :: a
end subroutine sub_optional

subroutine sub_target(a)
  use, intrinsic :: iso_c_binding
  type(*), target :: a
end subroutine sub_target

subroutine sub_volatile(a)
  use, intrinsic :: iso_c_binding
  type(*), volatile :: a
end subroutine sub_volatile
