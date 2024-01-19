!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : March 2011
!*
!*  DESCRIPTION
!*
!*  Assign constants to Derived Type coarray components
!*  (scalars and arrays of different kinds)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module consts
	integer(1), parameter :: min1 = -huge(0_1)-1,  max1 = huge(0_1)
	integer(4), parameter :: min4 = -huge(0_4)-1,  max4 = huge(0_4)
end module


program DTaccess_a008
	use consts
    implicit none

	type DT
		integer(1) :: i1
		integer(2) :: i2
		integer(4) :: i4
		integer(8) :: i8
	end type

    integer(2), parameter :: min2 = -huge(0_2)-1
    integer(8), parameter :: min8 = -huge(0_8)-1
	type (DT), save :: caf[0:*], cafar(10)[1,*]


    caf%i1 = min1
    caf%i2 = min2
    caf%i4 = min4
    caf%i8 = min8

    cafar%i1 = [1,1,2,3,5,8,13,21,34,min1]
    cafar(1:9:2)%i2 = min2 					! odd elements
    cafar([2,4,6,8,10])%i4 = min4 			! even elements
    cafar(9:1:-2)%i8 = min8 				! odd elements again

    if ( (caf%i1 /= min1) .or. (caf%i2 /= min2) .or. (caf%i4 /= min4) .or. (caf%i8 /= min8) ) then
    	print *, caf
    	error stop 21
    end if

    if ( any(cafar%i1 /= [1,1,2,3,5,8,13,21,34,min1]) ) then
    	print *, cafar(:)%i1
    	error stop 22
    end if

    if ( any(cafar(:)%i2 /= [min2,0_2,min2,0_2,min2,0_2,min2,0_2,min2,0_2]) ) then
    	print *, cafar(:)%i2
    	error stop 23
    end if

    if ( any(cafar(:)%i4 /= [0_4,min4,0_4,min4,0_4,min4,0_4,min4,0_4,min4]) ) then
    	print *, cafar(:)%i4
    	error stop 24
    end if

    if ( any(cafar(:)%i8 /= [min8,0_8,min8,0_8,min8,0_8,min8,0_8,min8,0_8]) ) then
    	print *, cafar(:)%i8
    	error stop 25
    end if

    call sub0()
end


subroutine sub0()
	use consts
	integer(2), parameter :: max2 = huge(0_2)
    integer(8), parameter :: max8 = huge(0_8)

	type DT
		integer*1 :: i1
		integer*2 :: i2
		integer*4 :: i4
		integer*8 :: i8
	end type
	type (DT), save :: caf[*], cafar[*]
	caf = DT(0,0,0,0)


    caf = DT(max1, max2, max4, max8)

    if ( (caf%i1 /= max1) .or. (caf%i2 /= max2) .or. (caf%i4 /= max4) .or. (caf%i8 /= max8) ) then
    	print *, caf
    	error stop 31
    end if
end subroutine
