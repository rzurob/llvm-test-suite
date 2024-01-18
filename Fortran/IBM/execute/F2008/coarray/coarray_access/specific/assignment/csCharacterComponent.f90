!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-10-04
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray access (specific) - assignment
!*  SECONDARY FUNCTIONS TESTED : in main program, assign character component of derived type variables to coarray variables and vice-versa
!*  ADAPTED FROM               : csSimpleCharacter, csIntegerComponent
!*
!*  DESCRIPTION
!*
!*  Create derived type values with character components and assign them to character
!*  coarray scalars and arrays of different kinds in the main program (and vice versa).
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module mod
    implicit none
    type dt
       character(1) :: f1
       character(3) :: f2
    end type dt

    interface operator(.ne.)
      module procedure notSameDT
    end interface

contains

  elemental type(dt) function ctor(j1,j3)
    character(1) , intent(in) ::  j1
    character(3) , intent(in) ::  j3
    ctor = dt(j1,j3)
  end function ctor

  elemental logical function notSameDT(a,b)
    type(dt), intent(in) :: a, b
    notSameDT = a%f1 /= b%f1 .or. a%f2 /= b%f2
  end function notSameDT

end module mod

program csCharacterComponent

  use :: mod
  implicit none

  character(1), parameter :: a1 = ' ',    b1 = '~'
  character(3), parameter :: a3 = 'A9Z',  b3 = '!z~', hash = '###'
  integer, parameter :: A_CAP = iachar('A'), Z_LWR = iachar('z')
  integer :: i

  type(dt) :: v, a(10), vtmp, atmp(10)

  character(1), save :: c1[*] = '', ca1(10)[*] = ''
  character(3), save :: c3[*] = '', ca3(10)[*] = ''

  v = dt(a1, a3)
  a = [(dt(achar(A_CAP+i-1),repeat(achar(Z_LWR-i+1),3)),i=1,10)]

  c1 = v%f1
  c3 = v%f2

  ca1 = a%f1
  ca3 = a%f2

  if (c1/=a1 .or. c3/=a3) error stop 2
  if (any(ca1 /= [(achar(A_CAP+i-1),i=1,10)])) error stop 3
  if (any(ca3 /= [(repeat(achar(Z_LWR-i+1),3),i=1,10)])) error stop 4

  ! now test assignment *from* coarrays
  vtmp = dt(c1,c3)
  atmp = ctor(ca1,ca3)

  if (vtmp /= v) error stop 12
  if (any(atmp /= a)) error stop 13

end program csCharacterComponent
