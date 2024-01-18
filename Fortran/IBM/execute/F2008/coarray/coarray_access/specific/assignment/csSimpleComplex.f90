!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : csSimpleComplex
!*
!*  PROGRAMMER                 : dforster
!*  DATE                       : 2010-10-04
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray access (specific) - assignment
!*  SECONDARY FUNCTIONS TESTED : assign COMPLEX noncoarray variables to coarray variables (scalar and array) in main program and vice-versa
!*  ADAPTED FROM               : csSimpleReal (<-csSimpleInteger<-csSimpleLogical)
!*
!*  DESCRIPTION
!*
!*  Assign simple values to complex coarray scalars and arrays of different kinds
!*  in the main program.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program csSimpleComplex

    use ieee_arithmetic
    implicit none

    complex(4), parameter :: a4 = (tiny(0.0_4),-huge(0.0_4)), b4 = (huge(0.0_4),0.76543E21)
    complex(8), parameter :: a8 = (-tiny(0.0_8),huge(0.0_8)), b8 = (-0.123456789012D123,tiny(0.0_8))

    complex(4), save :: z4[*] = 0, za4(10)[*] = 0
    complex(8), save :: z8[*] = 0, za8(10)[*] = 0

    complex(4) :: v4, ztmp4, zatmp4(10)
    complex(8) :: v8, ztmp8, zatmp8(10)

    ! Try to avoid optimisations (v? are quasi-constant: since we don't run tests
    ! which provide command-line arguments, they will always be assigned the values
    ! below, but the optimiser can't know that).
    v4 = 0.0
    v8 = 0.0D0
    if (command_argument_count() < 10) then
      v4 = a4
      v8 = a8
    end if

    ! start with the minimum value for each kind:
    z4 = v4
    z8 = v8

    za4([2,4,6,8,10]) = v4 ! even elements
    za8(9:1:-2)       = v8 ! odd elements again

    ! to keep assignment and argument association distinct, we assign the coarray value to
    ! a temporary variable and pass that in to the test procedures
    ztmp4 = z4
    ztmp8 = z8
    if (.not.same4z(ztmp4,v4) .or. .not.same8z(ztmp8,v8)) then
        print *, z4, ztmp4, v4, z8, ztmp8, v8
        error stop 2
    end if

    zatmp4 = za4
    if (.not. all(same4z(zatmp4,[0.0E0,v4,0.0E0,v4,0.0E0,v4,0.0E0,v4,0.0E0,v4]))) then
        print *, za4
        print *, zatmp4
        print *, [0.0E0,v4,0.0E0,v4,0.0E0,v4,0.0E0,v4,0.0E0,v4]
        error stop 3
    end if

    zatmp8 = za8
    if (.not. all(same8z(zatmp8,[v8,0.0D0,v8,0.0D0,v8,0.0D0,v8,0.0D0,v8,0.0D0])))  then
        print *, za8
        print *, zatmp8
        print *, [v8,0.0D0,v8,0.0D0,v8,0.0D0,v8,0.0D0,v8,0.0D0]
        error stop 4
    end if

    ! now set to max value
    if (command_argument_count() < 10) then
      v4 = b4
      v8 = b8
    end if

    z4 = v4
    z8 = v8

    za4 = a4
    za4([2,4,6,8,10]) = v4 ! even elements
    za8 = a8
    za8(9:1:-2)       = v8 ! odd elements again

    ztmp4 = z4
    ztmp8 = z8
    if (.not.same4z(ztmp4,v4) .or. .not.same8z(ztmp8,v8)) then
        print *, z4, ztmp4, v4, z8, ztmp8, v8
        error stop 5
    end if

    zatmp4 = za4
    if (.not. all(same4z(zatmp4,[a4,v4,a4,v4,a4,v4,a4,v4,a4,v4]))) then
        print *, za4
        print *, zatmp4
        print *, [a4,v4,a4,v4,a4,v4,a4,v4,a4,v4]
        error stop 6
    end if

    zatmp8 = za8
    if (.not. all(same8z(zatmp8,[v8,a8,v8,a8,v8,a8,v8,a8,v8,a8])))  then
        print *, za8
        print *, zatmp8
        print *, [v8,a8,v8,a8,v8,a8,v8,a8,v8,a8]
        error stop 7
    end if

contains

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

end program csSimpleComplex
