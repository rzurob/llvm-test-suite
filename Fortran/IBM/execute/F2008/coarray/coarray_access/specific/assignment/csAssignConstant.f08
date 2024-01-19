!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-10-04
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray access (specific) - assignment
!*  SECONDARY FUNCTIONS TESTED : assign constants of various intrinsic types to coarray variables (scalar and array) in main program
!*  ADAPTED FROM               : csSimpleInteger (<-csSimpleLogical)
!*
!*  DESCRIPTION
!*
!*  Assign constants to integer coarray scalars and arrays of different kinds
!*  in the main program.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program csAssignConstant

    implicit none

    ! we're relying on the use of 2's complement arithmetic when we use an expression
    ! like "-huge(0_KIND)-1"
    integer(1), parameter :: min1 = -huge(0_1)-1,  max1 = huge(0_1), mid1 = 13_1
    integer(2), parameter :: min2 = -huge(0_2)-1,  max2 = huge(0_2), mid2 = 12345_2
    integer(4), parameter :: min4 = -huge(0_4)-1,  max4 = huge(0_4), mid4 = 898973451_4
    integer(8), parameter :: min8 = -huge(0_8)-1,  max8 = huge(0_8), mid8 = -12345678901234_8

    integer(1) :: min1, itmp1, iatmp1(10)
    integer(2) :: v2, itmp2, iatmp2(10)
    integer(4) :: v4, itmp4, iatmp4(10)
    integer(8) :: v8, itmp8, iatmp8(10)

    integer(1), save :: i1[*] = 0, ia1(10)[*] = 0
    integer(2), save :: i2[*] = 0, ia2(10)[*] = 0
    integer(4), save :: i4[*] = 0, ia4(10)[*] = 0
    integer(8), save :: i8[*] = 0, ia8(10)[*] = 0

    ! start with the minimum value for each kind:
    i1 = min1
    i2 = min2
    i4 = min4
    i8 = min8

    ia1               = [1,1,2,3,5,8,13,21,34,min1]
    ia2(1:9:2)        = min2 ! odd elements
    ia4([2,4,6,8,10]) = min4 ! even elements
    ia8(9:1:-2)       = min8 ! odd elements again

    if (i1/=min1 .or. i2/=min2 .or. i4/=min4 .or. i8/=min8) error stop 2
    if (any(ia1 /= [1,1,2,3,5,8,13,21,34,min1])) error stop 3
    if (any(ia2 /= [min2,0,min2,0,min2,0,min2,0,min2,0])) error stop 4
    if (any(ia4 /= [0,min4,0,min4,0,min4,0,min4,0,min4])) error stop 5
    if (any(ia8 /= [min8,0,min8,0,min8,0,min8,0,min8,0])) error stop 6

    ! now test assignment *from* coarrays
    itmp1 = i1
    itmp2 = i2
    itmp4 = i4
    itmp8 = i8
    iatmp1 = ia1
    iatmp2 = ia2
    iatmp4 = ia4
    iatmp8 = ia8

    if (itmp1/=min1 .or. itmp2/=min2 .or. itmp4/=min4 .or. itmp8/=min8) error stop 12
    if (any(iatmp1 /= [1,1,2,3,5,8,13,21,34,min1])) error stop 13
    if (any(iatmp2 /= [min2,0,min2,0,min2,0,min2,0,min2,0])) error stop 14
    if (any(iatmp4 /= [0,min4,0,min4,0,min4,0,min4,0,min4])) error stop 15
    if (any(iatmp8 /= [min8,0,min8,0,min8,0,min8,0,min8,0])) error stop 16

    ! now set to max value

    i1 = max1
    i2 = max2
    i4 = max4
    i8 = max8

    ! arrays will have some intermediate values, some max values
    ia1               = [1,1,2,3,5,8,13,21,34,max1]
    ia2               = mid2
    ia2(1:9:2)        = max2 ! odd elements
    ia4               = mid4
    ia4([2,4,6,8,10]) = max4 ! even elements
    ia8               = mid8
    ia8(9:1:-2)       = max8 ! odd elements again

    if (i1/=max1 .or. i2/=max2 .or. i4/=max4 .or. i8/=max8) error stop 22
    if (any(ia1 /= [1,1,2,3,5,8,13,21,34,max1])) error stop 23
    if (any(ia2 /= [max2,mid2,max2,mid2,max2,mid2,max2,mid2,max2,mid2])) error stop 24
    if (any(ia4 /= [mid4,max4,mid4,max4,mid4,max4,mid4,max4,mid4,max4])) error stop 25
    if (any(ia8 /= [max8,mid8,max8,mid8,max8,mid8,max8,mid8,max8,mid8])) error stop 26

    itmp1 = i1
    itmp2 = i2
    itmp4 = i4
    itmp8 = i8
    iatmp1 = ia1
    iatmp2 = ia2
    iatmp4 = ia4
    iatmp8 = ia8

    if (itmp1/=max1 .or. itmp2/=max2 .or. itmp4/=max4 .or. itmp8/=max8) error stop 32
    if (any(iatmp1 /= [1,1,2,3,5,8,13,21,34,max1])) error stop 33
    if (any(iatmp2 /= [max2,mid2,max2,mid2,max2,mid2,max2,mid2,max2,mid2])) error stop 34
    if (any(iatmp4 /= [mid4,max4,mid4,max4,mid4,max4,mid4,max4,mid4,max4])) error stop 35
    if (any(iatmp8 /= [max8,mid8,max8,mid8,max8,mid8,max8,mid8,max8,mid8])) error stop 36

end program csAssignConstant
