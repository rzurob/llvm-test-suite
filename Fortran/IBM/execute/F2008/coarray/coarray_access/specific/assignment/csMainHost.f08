!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-10-04
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray access (specific) - assignment
!*  SECONDARY FUNCTIONS TESTED : in procedures internal to main program, assign host-associated constants and variables of various intrinsic types to host-associated coarray variables and assign coarray variables to host-associated variables
!*  ADAPTED FROM               : csSimpleInteger (<-csSimpleLogical)
!*
!*  DESCRIPTION
!*
!*  Assign local values to host-associated integer coarray scalars and arrays of
!*  different kinds in an internal subroutine.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program csMainHost

    implicit none

    ! we're relying on the use of 2's complement arithmetic when we use an expression
    ! like "-huge(0_KIND)-1"
    integer(1), parameter :: min1 = -huge(0_1)-1,  max1 = huge(0_1), mid1 = 13_1
    integer(2), parameter :: min2 = -huge(0_2)-1,  max2 = huge(0_2), mid2 = 12345_2
    integer(4), parameter :: min4 = -huge(0_4)-1,  max4 = huge(0_4), mid4 = 898973451_4
    integer(8), parameter :: min8 = -huge(0_8)-1,  max8 = huge(0_8), mid8 = -12345678901234_8

    integer(1), save :: i1[*] = 0, ia1(10)[*] = 0
    integer(2), save :: i2[*] = 0, ia2(10)[*] = 0
    integer(4), save :: i4[*] = 0, ia4(10)[*] = 0
    integer(8), save :: i8[*] = 0, ia8(10)[*] = 0

    call sub

contains

  subroutine sub

    integer(1) :: v1, itmp1, iatmp1(10)
    integer(2) :: v2, itmp2, iatmp2(10)
    integer(4) :: v4, itmp4, iatmp4(10)
    integer(8) :: v8, itmp8, iatmp8(10)

    ! Try to avoid optimisations (v? are quasi-constant: since we don't run tests
    ! which provide command-line arguments, they will always be assigned the values
    ! below, but the optimiser can't know that).
    v1 = 0
    v2 = 0
    v4 = 0
    v8 = 0
    if (command_argument_count() < 10) then
      v1 = min1
      v2 = min2
      v4 = min4
      v8 = min8
    end if

    ! start with the minimum value for each kind:
    i1 = v1
    i2 = v2
    i4 = v4
    i8 = v8

    ia1               = [1,1,2,3,5,8,13,21,34,v1]
    ia2(1:9:2)        = v2 ! odd elements
    ia4([2,4,6,8,10]) = v4 ! even elements
    ia8(9:1:-2)       = v8 ! odd elements again

    if (i1/=v1 .or. i2/=v2 .or. i4/=v4 .or. i8/=v8) error stop 2
    if (any(ia1 /= [1,1,2,3,5,8,13,21,34,v1])) error stop 3
    if (any(ia2 /= [v2,0,v2,0,v2,0,v2,0,v2,0])) error stop 4
    if (any(ia4 /= [0,v4,0,v4,0,v4,0,v4,0,v4])) error stop 5
    if (any(ia8 /= [v8,0,v8,0,v8,0,v8,0,v8,0])) error stop 6

    ! now test assignment *from* coarrays
    itmp1 = i1
    itmp2 = i2
    itmp4 = i4
    itmp8 = i8
    iatmp1 = ia1
    iatmp2 = ia2
    iatmp4 = ia4
    iatmp8 = ia8

    if (itmp1/=v1 .or. itmp2/=v2 .or. itmp4/=v4 .or. itmp8/=v8) error stop 12
    if (any(iatmp1 /= [1,1,2,3,5,8,13,21,34,v1])) error stop 13
    if (any(iatmp2 /= [v2,0,v2,0,v2,0,v2,0,v2,0])) error stop 14
    if (any(iatmp4 /= [0,v4,0,v4,0,v4,0,v4,0,v4])) error stop 15
    if (any(iatmp8 /= [v8,0,v8,0,v8,0,v8,0,v8,0])) error stop 16

    ! now set to max value
    if (command_argument_count() < 10) then
      v1 = max1
      v2 = max2
      v4 = max4
      v8 = max8
    end if

    i1 = v1
    i2 = v2
    i4 = v4
    i8 = v8

    ! arrays will have some intermediate values, some max values
    ia1               = [1,1,2,3,5,8,13,21,34,v1]
    ia2               = mid2
    ia2(1:9:2)        = v2 ! odd elements
    ia4               = mid4
    ia4([2,4,6,8,10]) = v4 ! even elements
    ia8               = mid8
    ia8(9:1:-2)       = v8 ! odd elements again

    if (i1/=v1 .or. i2/=v2 .or. i4/=v4 .or. i8/=v8) error stop 22
    if (any(ia1 /= [1,1,2,3,5,8,13,21,34,v1])) error stop 23
    if (any(ia2 /= [v2,mid2,v2,mid2,v2,mid2,v2,mid2,v2,mid2])) error stop 24
    if (any(ia4 /= [mid4,v4,mid4,v4,mid4,v4,mid4,v4,mid4,v4])) error stop 25
    if (any(ia8 /= [v8,mid8,v8,mid8,v8,mid8,v8,mid8,v8,mid8])) error stop 26

    itmp1 = i1
    itmp2 = i2
    itmp4 = i4
    itmp8 = i8
    iatmp1 = ia1
    iatmp2 = ia2
    iatmp4 = ia4
    iatmp8 = ia8

    if (itmp1/=v1 .or. itmp2/=v2 .or. itmp4/=v4 .or. itmp8/=v8) error stop 32
    if (any(iatmp1 /= [1,1,2,3,5,8,13,21,34,v1])) error stop 33
    if (any(iatmp2 /= [v2,mid2,v2,mid2,v2,mid2,v2,mid2,v2,mid2])) error stop 34
    if (any(iatmp4 /= [mid4,v4,mid4,v4,mid4,v4,mid4,v4,mid4,v4])) error stop 35
    if (any(iatmp8 /= [v8,mid8,v8,mid8,v8,mid8,v8,mid8,v8,mid8])) error stop 36

  end subroutine sub

end program csMainHost
