!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-10-04
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray access (specific) - assignment
!*  SECONDARY FUNCTIONS TESTED : assign REAL noncoarray variables to coarray variables (scalar and array) in main program and vice-versa
!*  ADAPTED FROM               : csSimpleInteger (<-csSimpleLogical)
!*
!*  DESCRIPTION
!*
!*  Assign simple values to real coarray scalars and arrays of different kinds
!*  in the main program.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program csSimpleReal

    use ieee_arithmetic
    implicit none

    real(4), parameter :: min4 = tiny(0.0_4),  max4 = huge(0.0_4), mid4 = 0.76543E21
    real(8), parameter :: min8 = tiny(0.0_8),  max8 = huge(0.0_8), mid8 =-0.123456789012D123

    real(4), save :: r4[*] = 0, ra4(10)[*] = 0
    real(8), save :: r8[*] = 0, ra8(10)[*] = 0

    real(4) :: v4, rtmp4, ratmp4(10)
    real(8) :: v8, rtmp8, ratmp8(10)

    ! Try to avoid optimisations (v? are quasi-constant: since we don't run tests
    ! which provide command-line arguments, they will always be assigned the values
    ! below, but the optimiser can't know that).
    v4 = 0.0
    v8 = 0.0D0
    if (command_argument_count() < 10) then
      v4 = min4
      v8 = min8
    end if

    ! start with the minimum value for each kind:
    r4 = v4
    r8 = v8

    ra4([2,4,6,8,10]) = v4 ! even elements
    ra8(9:1:-2)       = v8 ! odd elements again

    ! to keep assignment and argument association distinct, we assign the coarray value to
    ! a temporary variable and pass that in to the test procedures
    rtmp4 = r4
    rtmp8 = r8
    if (.not.same4(rtmp4,v4) .or. .not.same8(rtmp8,v8)) then
        print *, r4, rtmp4, v4, r8, rtmp8, v8
        error stop 2
    end if

    ratmp4 = ra4
    if (.not. all(same4(ratmp4,[0.0E0,v4,0.0E0,v4,0.0E0,v4,0.0E0,v4,0.0E0,v4]))) then
        print *, ra4
        print *, ratmp4
        print *, [0.0E0,v4,0.0E0,v4,0.0E0,v4,0.0E0,v4,0.0E0,v4]
        error stop 3
    end if

    ratmp8 = ra8
    if (.not. all(same8(ratmp8,[v8,0.0D0,v8,0.0D0,v8,0.0D0,v8,0.0D0,v8,0.0D0])))  then
        print *, ra8
        print *, ratmp8
        print *, [v8,0.0D0,v8,0.0D0,v8,0.0D0,v8,0.0D0,v8,0.0D0]
        error stop 4
    end if

    ! now set to max value
    if (command_argument_count() < 10) then
      v4 = max4
      v8 = max8
    end if

    r4 = v4
    r8 = v8

    ra4 = mid4
    ra4([2,4,6,8,10]) = v4 ! even elements
    ra8 = mid8
    ra8(9:1:-2)       = v8 ! odd elements again

    rtmp4 = r4
    rtmp8 = r8
    if (.not.same4(rtmp4,v4) .or. .not.same8(rtmp8,v8)) then
        print *, r4, rtmp4, v4, r8, rtmp8, v8
        error stop 5
    end if

    ratmp4 = ra4
    if (.not. all(same4(ratmp4,[mid4,v4,mid4,v4,mid4,v4,mid4,v4,mid4,v4]))) then
        print *, ra4
        print *, ratmp4
        print *, [mid4,v4,mid4,v4,mid4,v4,mid4,v4,mid4,v4]
        error stop 6
    end if

    ratmp8 = ra8
    if (.not. all(same8(ratmp8,[v8,mid8,v8,mid8,v8,mid8,v8,mid8,v8,mid8])))  then
        print *, ra8
        print *, ratmp8
        print *, [v8,mid8,v8,mid8,v8,mid8,v8,mid8,v8,mid8]
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

end program csSimpleReal
