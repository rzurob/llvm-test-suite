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
!*  DESCRIPTION                : non-BIND(C) procedures defined in Fortran,
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

  contains
  subroutine mod_sub(a)
     type(*) :: a
  end subroutine mod_sub

  subroutine mod_sub_optional(a)
     type(*), optional :: a
  end subroutine mod_sub_optional

  subroutine mod_sub_target(a)
     type(*), target :: a
  end subroutine mod_sub_target

  subroutine mod_sub_volatile(a)
     type(*), volatile :: a
  end subroutine mod_sub_volatile
end module mod

program AssumedType14f
use mod
implicit none

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

call mod_sub(b)
call mod_sub(c)
call mod_sub(dt1_0)
call mod_sub(dt2_0)
call mod_sub(dt3_0)

call mod_sub_optional(b)
call mod_sub_optional(c)
call mod_sub_optional(dt1_0)
call mod_sub_optional(dt2_0)
call mod_sub_optional(dt3_0)

call mod_sub_target(b)
call mod_sub_target(c)
call mod_sub_target(dt1_0)
call mod_sub_target(dt2_0)
call mod_sub_target(dt3_0)

call mod_sub_volatile(b)
call mod_sub_volatile(c)
call mod_sub_volatile(dt1_0)
call mod_sub_volatile(dt2_0)
call mod_sub_volatile(dt3_0)

  contains
  subroutine sub(a)
     type(*) :: a
  end subroutine sub

  subroutine sub_optional(a)
     type(*), optional :: a
  end subroutine sub_optional

  subroutine sub_target(a)
     type(*), target :: a
  end subroutine sub_target

  subroutine sub_volatile(a)
     type(*), volatile :: a
  end subroutine sub_volatile
end program AssumedType14f
