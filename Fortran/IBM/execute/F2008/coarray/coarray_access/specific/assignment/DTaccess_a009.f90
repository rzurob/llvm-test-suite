!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : DTaccess_a009.f
!*
!*  PROGRAMMER                 : Francesco Cassullo
!*  DATE                       : March 2011
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  DESCRIPTION
!*
!*  Assign values from common block variables to a Derived Type coarray components
!*  (scalars and arrays of different kinds)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program testing

    implicit none

	type DT
		integer*4 :: i4
		integer*8 :: i8
	end type

    integer*4, parameter :: const4 = 4
    integer*8, parameter :: const8 = 8

    integer*4 :: v4, iatmp4(10)
    integer*8 :: v8, iatmp8(10)

    common /com_data/ v4, iatmp4, v8, iatmp8

    type(DT), save, codimension[*] :: caf, cafar(10)

    v4 = const4
    v8 = const8
    iatmp4 = [integer*1 :: 1,1,2,3,5,8,13,21,34,v4]
    iatmp8 = [integer*8 :: v8,0,v8,0,v8,0,v8,0,v8,0]

    ! test assignment to coarrays
	call sub0()

    ! now test assignment from coarrays
    caf%i4 = 1
    cafar(:)%i4 = 1
    caf%i8 = 2
    cafar(:)%i8 = 2
    v4 = caf%i4
    v8 = caf%i8
    iatmp4 = cafar%i4
    iatmp8 = cafar%i8

    if ( (v4 /= 1) .or. (v8 /= 2) ) then
    	print *, v4, v8
    	error stop 15
    end if
    
    if ( any(iatmp4 /= [integer*4 :: 1,1,1,1,1,1,1,1,1,1]) ) error stop 16
    if ( any(iatmp8 /= [integer*8 :: 2,2,2,2,2,2,2,2,2,2]) ) error stop 17

end program testing


subroutine sub0()
	type DT
		integer*4 :: i4
		integer*8 :: i8
	end type
	type(DT), save :: caf[2,2,*], cafar(10)[0:1,0:1,0:*]

    integer*4, parameter :: c4 = 4
    integer*8, parameter :: c8 = 8

    integer*4 :: v4, iatmp4(10)
    integer*8 :: v8, iatmp8(10)

    common /com_data/ v4, iatmp4, v8, iatmp8

    
    caf%i4 = v4
    caf%i8 = v8
    cafar%i4 = iatmp4
    cafar%i8 = iatmp8

    if ( (caf%i4 /= c4) .or. (caf%i8 /= c8) ) then
    	print *, caf
    	error stop 21
    end if
    
    if ( any(cafar%i4 /= [1,1,2,3,5,8,13,21,34,v4]) ) then
    	print *, cafar(:)%i4
    	error stop 22
    end if
    
    if ( any(cafar%i8 /= [v8,0,v8,0,v8,0,v8,0,v8,0]) ) then
    	print *, cafar(:)%i8 
    	error stop 23
    end if

end subroutine

