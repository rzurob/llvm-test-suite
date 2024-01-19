!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-10-04
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray access (specific) - procedure call
!*  SECONDARY FUNCTIONS TESTED : pass REAL scalar coarray actual argument to procedure which examines it and modifies it
!*  ADAPTED FROM               : csSimpleReal, csIntegerArg
!*
!*  DESCRIPTION
!*
!*  Invoke procedures with coarray actual arguments - they examine and modify the arguments.
!*  Arrays are already handled in many other features, so we skip these.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program csRealArg

    use ieee_arithmetic
    implicit none

    real(4), parameter :: min4 = tiny(0.0_4),  max4 = huge(0.0_4), mid4 = 0.76543E21
    real(8), parameter :: min8 = tiny(0.0_8),  max8 = huge(0.0_8), mid8 =-0.123456789012D123

    real(4), save :: r4[*] = 0
    real(8), save :: r8[*] = 0

    call twiddle4(r4, 0.0_4, min4, 21)
    call twiddle4(r4, min4, mid4, 22)
    call twaddle4(r4, mid4, max4, 23)
    call twaddle4(r4, max4, min4, 24)
    if (.not. same4(min4,fiddle4(r4,mid4))) call fail(25)
    if (.not. same4(mid4,faddle4(r4,max4))) call fail(26)

    call twiddle8(r8, 0.0_8, min8, 31)
    call twiddle8(r8, min8, mid8, 32)
    call twaddle8(r8, mid8, max8, 33)
    call twaddle8(r8, max8, min8, 34)
    if (.not. same8(min8,fiddle8(r8,mid8))) call fail(35)
    if (.not. same8(mid8,faddle8(r8,max8))) call fail(36)

contains

  subroutine twiddle4(a4, exp4, new4, nr)
    real(4) :: a4, exp4, new4
    integer :: nr
    if (.not. same4(a4,exp4)) call fail(nr)
    a4 = new4
  end subroutine twiddle4

  subroutine twaddle4(a4, exp4, new4, nr)
    real(4) :: a4[*], exp4, new4
    integer :: nr
    if (.not. same4(a4,exp4)) call fail(nr)
    a4 = new4
  end subroutine twaddle4

  real(4) function fiddle4(a4, new4)
    real(4) :: a4, new4
    fiddle4 = a4
    a4 = new4
  end function fiddle4

  real(4) function faddle4(a4, new4)
    real(4) :: a4[*], new4
    faddle4 = a4
    a4 = new4
  end function faddle4


  subroutine twiddle8(a8, exp8, new8, nr)
    real(8) :: a8, exp8, new8
    integer :: nr
    if (.not. same8(a8,exp8)) call fail(nr)
    a8 = new8
  end subroutine twiddle8

  subroutine twaddle8(a8, exp8, new8, nr)
    real(8) :: a8[*], exp8, new8
    integer :: nr
    if (.not. same8(a8,exp8)) call fail(nr)
    a8 = new8
  end subroutine twaddle8

  real(8) function fiddle8(a8, new8)
    real(8) :: a8, new8
    fiddle8 = a8
    a8 = new8
  end function fiddle8

  real(8) function faddle8(a8, new8)
    real(8) :: a8[*], new8
    faddle8 = a8
    a8 = new8
  end function faddle8

  subroutine fail(nr)
    integer :: nr
    print *, "Failed in test", nr
    error stop 12
  end subroutine fail

  elemental logical function same4(a1,a2)
    real(4), intent(in) :: a1, a2
    real(4) :: r1, r2
    same4 = .true.
    r1 = a1
    r2 = a2
    ! covers exact equality, Inf and NaN:
    if (r1 == r2 .or. ieee_is_nan(r1) .and. ieee_is_nan(r1)) return
    if (.not.ieee_is_normal(r1) .or. .not.ieee_is_normal(r2)) then
      r1 = 1e20 * r1
      r2 = 1e20 * r2
    end if
    ! covers approximate equality:
    same4 = abs(r1 - r2) <= abs((r1*0.5E-5 + r2*0.5E-5)) ! avoiding overflow on max
  end function same4

  elemental logical function same8(a1,a2)
    real(8), intent(in) :: a1, a2
    real(8) :: r1, r2
    same8 = .true.
    r1 = a1
    r2 = a2
    ! covers exact equality, Inf and NaN:
    if (r1 == r2 .or. ieee_is_nan(r1) .and. ieee_is_nan(r1)) return
    if (.not.ieee_is_normal(r1) .or. .not.ieee_is_normal(r2)) then
      r1 = 1e40 * r1
      r2 = 1e40 * r2
    end if
    ! covers approximate equality:
    same8 = abs(r1 - r2) <= abs((r1*0.5D-14 + r2*0.5D-14)) ! avoiding overflow on max
  end function same8

end program csRealArg
