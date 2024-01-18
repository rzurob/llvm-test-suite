!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : csIntegerComponent
!*
!*  PROGRAMMER                 : dforster
!*  DATE                       : 2010-10-04
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray access (specific) - assignment
!*  SECONDARY FUNCTIONS TESTED : in main program, assign integer component of derived type variables to coarray variables and vice-versa
!*  ADAPTED FROM               : csSimpleInteger (<-csSimpleLogical)
!*
!*  DESCRIPTION
!*
!*  Assign simple values to integer coarray scalars and arrays of different kinds
!*  in the main program.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module mod
    implicit none
    type dt
       integer(1) :: f1
       integer(2) :: f2
       integer(4) :: f4
       integer(8) :: f8
    end type dt

    interface operator(.ne.)
      module procedure notSameDT
    end interface

contains

  elemental type(dt) function ctor(j1,j2,j4,j8)
    integer(1) , intent(in) ::  j1
    integer(2) , intent(in) ::  j2
    integer(4) , intent(in) ::  j4
    integer(8) , intent(in) ::  j8
    ctor = dt(j1,j2,j4,j8)
  end function ctor

  elemental logical function notSameDT(a,b)
    type(dt), intent(in) :: a, b
    notSameDT = a%f1 /= b%f1 .or. a%f2 /= b%f2 .or. a%f4 /= b%f4 .or. a%f8 /= b%f8
  end function notSameDT

end module mod


program csIntegerComponent

  use :: mod
  implicit none

  type(dt) :: v, vtmp, a(10), atmp(10)

  ! we're relying on the use of 2's complement arithmetic when we use an expression
  ! like "-huge(0_KIND)-1"
  integer(1), parameter :: par1 = -huge(0_1)-1
  integer(2), parameter :: par2 = huge(0_2)
  integer(4), parameter :: par4 = 898973451_4
  integer(8), parameter :: par8 = huge(0_8)

  integer(1), save :: i1[*] = 0, ia1(10)[*] = 0
  integer(2), save :: i2[*] = 0, ia2(10)[*] = 0
  integer(4), save :: i4[*] = 0, ia4(10)[*] = 0
  integer(8), save :: i8[*] = 0, ia8(10)[*] = 0

  integer :: i

  v = dt(par1, par2, par4, par8)
  a = [(dt(i,256*i,100000*i,10000000000_8*i),i=1,10)]

  i1 = v%f1
  i2 = v%f2
  i4 = v%f4
  i8 = v%f8

  ia1 = a%f1
  ia2 = a%f2
  ia4 = a%f4
  ia8 = a%f8

  if (i1/=par1 .or. i2/=par2 .or. i4/=par4 .or. i8/=par8) error stop 2
  if (any(ia1 /= [(i,i=1,10)])) error stop 3
  if (any(ia2 /= [(256*i,i=1,10)])) error stop 4
  if (any(ia4 /= [(100000*i,i=1,10)])) error stop 5
  if (any(ia8 /= [(10000000000_8*i,i=1,10)])) error stop 6

  ! now test assignment *from* coarrays
  vtmp = dt(i1,i2,i4,i8)
  atmp = ctor(ia1,ia2,ia4,ia8)

  if (vtmp /= v) error stop 12
  if (any(atmp /= a)) error stop 13

end program csIntegerComponent
