!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : csComplexArg
!*
!*  PROGRAMMER                 : dforster
!*  DATE                       : 2010-10-04
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray access (specific) - procedure call
!*  SECONDARY FUNCTIONS TESTED : pass COMPLEX scalar coarray actual argument to procedure which examines it and modifies it
!*  ADAPTED FROM               : csSimpleComplex (<-csSimpleReal<-csSimpleInteger<-csSimpleLogical)
!*
!*  DESCRIPTION
!*
!*  Invoke procedures with coarray actual arguments - they examine and modify the arguments.
!*  Arrays are already handled in many other features, so we skip these.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program csComplexArg

    use ieee_arithmetic
    implicit none

    complex(4), parameter :: a4 = (tiny(0.0_4),-huge(0.0_4)), b4 = (huge(0.0_4),0.76543E21)
    complex(8), parameter :: a8 = (-tiny(0.0_8),huge(0.0_8)), b8 = (-0.123456789012D123,tiny(0.0_8))

    complex(4), save :: z4[*] = 0
    complex(8), save :: z8[*] = 0

    call twiddle4(z4, (0.0_4,0.0_4), a4, 21)
    call twiddle4(z4, a4, b4, 22)
    call twaddle4(z4, b4, a4, 23)
    call twaddle4(z4, a4, b4, 24)
    if (.not. same4z(b4,fiddle4(z4,a4))) call fail(25)
    if (.not. same4z(a4,faddle4(z4,b4))) call fail(26)

    call twiddle8(z8, (0.0_8,0.0_8), a8, 31)
    call twiddle8(z8, a8, b8, 32)
    call twaddle8(z8, b8, a8, 33)
    call twaddle8(z8, a8, b8, 34)
    if (.not. same8z(b8,fiddle8(z8,a8))) call fail(35)
    if (.not. same8z(a8,faddle8(z8,b8))) call fail(36)

contains

  subroutine twiddle4(a4, exp4, new4, nr)
    complex(4) :: a4, exp4, new4
    integer :: nr
    if (.not. same4z(a4,exp4)) call fail(nr)
    a4 = new4
  end subroutine twiddle4

  subroutine twaddle4(a4, exp4, new4, nr)
    complex(4) :: a4[*], exp4, new4
    integer :: nr
    if (.not. same4z(a4,exp4)) call fail(nr)
    a4 = new4
  end subroutine twaddle4

  complex(4) function fiddle4(a4, new4)
    complex(4) :: a4, new4
    fiddle4 = a4
    a4 = new4
  end function fiddle4

  complex(4) function faddle4(a4, new4)
    complex(4) :: a4[*], new4
    faddle4 = a4
    a4 = new4
  end function faddle4


  subroutine twiddle8(a8, exp8, new8, nr)
    complex(8) :: a8, exp8, new8
    integer :: nr
    if (.not. same8z(a8,exp8)) call fail(nr)
    a8 = new8
  end subroutine twiddle8

  subroutine twaddle8(a8, exp8, new8, nr)
    complex(8) :: a8[*], exp8, new8
    integer :: nr
    if (.not. same8z(a8,exp8)) call fail(nr)
    a8 = new8
  end subroutine twaddle8

  complex(8) function fiddle8(a8, new8)
    complex(8) :: a8, new8
    fiddle8 = a8
    a8 = new8
  end function fiddle8

  complex(8) function faddle8(a8, new8)
    complex(8) :: a8[*], new8
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

  elemental logical function same4z(a1,a2)
    complex(4), intent(in) :: a1, a2
    same4z = same4(real(a1),real(a2)) .and. same4(aimag(a1),aimag(a2))
  end function same4z

  elemental logical function same8z(a1,a2)
    complex(8), intent(in) :: a1, a2
    same8z = same8(real(a1),real(a2)) .and. same8(aimag(a1),aimag(a2))
  end function same8z

end program csComplexArg
