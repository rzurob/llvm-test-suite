!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : March 2011
!*
!*  DESCRIPTION
!*
!*  Assign simple values to a Derived Type coarray's character components
!*  (scalars and arrays of different kinds)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main

	type obj
		character(1) :: c1
		character(3) :: c3
	end type

	character(1), parameter :: a1 = ' ',    b1 = '~'
	character(3), parameter :: a3 = 'A9Z',  b3 = '!z~', hash = '###'
	character(1) :: v1, ctmp1, catmp1(10)
	character(3) :: v3, ctmp3, catmp3(10)

	type (obj), save :: caf[*], cafar(10)[*]

	v1 = hash
	v3 = hash
	v1 = a1
	v3 = a3

	! start with the a value for each len:
	caf%c1 = v1
	caf%c3 = v3
	cafar(:)%c1 = [' ','!','@','$','0','5','9','A','z',v1]
	cafar(1:9:2)%c3 = v3 		! odd elements

	if ( (caf%c1 /= v1) .or. (caf%c3 /= v3) ) error stop 2
	if ( any(cafar(:)%c1 /= [' ','!','@','$','0','5','9','A','z',v1]) ) then
		print '("cafar:",9(a1,"/"),a1,";")', cafar(:)%c1
		print '("[ ]:",9(a1,"/"),a1,";")', [' ','!','@','$','0','5','9','A','z',v1]
		print *, cafar(:)%c1 /= [' ','!','@','$','0','5','9','A','z',v1]
		print *, any(cafar(:)%c1 /= [' ','!','@','$','0','5','9','A','z',v1])
		error stop 3
	end if
	if ( any(cafar(:)%c3 /= [v3,'   ',v3,'   ',v3,'   ',v3,'   ',v3,'   ']) ) then
		print '("cafar:",9(a3,"/"),a3,";")', cafar(:)%c3
		print '("[ ]:",9(a3,"/"),a3,";")', [v3,'   ',v3,'   ',v3,'   ',v3,'   ',v3,'   ']
		print *, cafar(:)%c3 /= [v3,'   ',v3,'   ',v3,'   ',v3,'   ',v3,'   ']
		print *, any(cafar(:)%c3 /= [v3,'   ',v3,'   ',v3,'   ',v3,'   ',v3,'   '])
		error stop 4
	end if

	! now test assignment *from* the coarrays:
	ctmp1 = caf%c1
	ctmp3 = ctmp3
	catmp1 = cafar(:)%c1
	catmp3 = cafar(:)%c3

	if ( (ctmp1 /= v1) .or. (ctmp3 /= v3) ) error stop 5
	if ( any(catmp1 /= [' ','!','@','$','0','5','9','A','z',v1]) ) then
		print '("catmp1:",9(a1,"/"),a1,";")', catmp1
		print '("   [ ]:",9(a1,"/"),a1,";")', [' ','!','@','$','0','5','9','A','z',v1]
		print *, catmp1 /= [' ','!','@','$','0','5','9','A','z',v1]
		print *, any(catmp1 /= [' ','!','@','$','0','5','9','A','z',v1])
		error stop 6
	end if
	if ( any(catmp3 /= [v3,'   ',v3,'   ',v3,'   ',v3,'   ',v3,'   ']) ) then
		print '("catmp3:",9(a3,"/"),a3,";")', catmp3
		print '("   [ ]:",9(a3,"/"),a3,";")', [v3,'   ',v3,'   ',v3,'   ',v3,'   ',v3,'   ']
		print *, catmp3 /= [v3,'   ',v3,'   ',v3,'   ',v3,'   ',v3,'   ']
		print *, any(catmp3 /= [v3,'   ',v3,'   ',v3,'   ',v3,'   ',v3,'   '])
		error stop 7
	end if


    ! now set to b value
	v1 = b1
	v3 = b3
	caf%c1 = v1
	caf%c3 = v3

	cafar(:)%c1 = hash
	cafar(1:9:2)%c1 = v1 	! odd elements
	cafar(:)%c3 = [' !@',v3,'^&*','()_','+AB','CXY','Zab','cdx','yz[','"]~']

	if ( (caf%c1 /= v1) .or. (caf%c3 /= v3) ) error stop 12
	if ( any(cafar(:)%c1 /= [character(1):: v1,hash,v1,hash,v1,hash,v1,hash,v1,hash]) ) then
		print '("cafar:",9(a1,"/"),a1,";")', cafar(:)%c1
		print '("[ ]:",9(a1,"/"),a1,";")', [character(1):: v1,hash,v1,hash,v1,hash,v1,hash,v1,hash]
		print *, cafar(:)%c1 /= [character(1):: v1,hash,v1,hash,v1,hash,v1,hash,v1,hash]
		print *, any(cafar(:)%c1 /= [character(1):: v1,hash,v1,hash,v1,hash,v1,hash,v1,hash])
		error stop 13
	end if
	if ( any(cafar(:)%c3 /= [' !@',v3,'^&*','()_','+AB','CXY','Zab','cdx','yz[','"]~']) ) then
		print '("cafar:",9(a3,"/"),a3,";")', cafar(:)%c3
		print '("[ ]:",9(a3,"/"),a3,";")', [' !@',v3,'^&*','()_','+AB','CXY','Zab','cdx','yz[','"]~']
		print *, cafar(:)%c3 /= [' !@',v3,'^&*','()_','+AB','CXY','Zab','cdx','yz[','"]~']
		print *, any(cafar(:)%c3 /= [' !@',v3,'^&*','()_','+AB','CXY','Zab','cdx','yz[','"]~'])
		error stop 14
	end if

	! now test assignment *from* the coarrays:
	ctmp1 = caf%c1
	ctmp3 = ctmp3
	catmp1 = cafar(:)%c1
	catmp3 = cafar(:)%c3

	if ( (ctmp1 /= v1) .or. (ctmp3 /= v3) ) error stop 15
	if ( any(catmp1 /= [character(1):: v1,hash,v1,hash,v1,hash,v1,hash,v1,hash]) ) then
		print '("catmp1:",9(a1,"/"),a1,";")', catmp1
		print '("   [ ]:",9(a1,"/"),a1,";")', [character(1):: v1,hash,v1,hash,v1,hash,v1,hash,v1,hash]
		print *, catmp1 /= [character(1):: v1,hash,v1,hash,v1,hash,v1,hash,v1,hash]
		print *, any(catmp1 /= [character(1):: v1,hash,v1,hash,v1,hash,v1,hash,v1,hash])
		error stop 16
	end if
	if ( any(catmp3 /= [' !@',v3,'^&*','()_','+AB','CXY','Zab','cdx','yz[','"]~']) ) then
		print '("catmp3:",9(a3,"/"),a3,";")', catmp3
		print '("   [ ]:",9(a3,"/"),a3,";")', [' !@',v3,'^&*','()_','+AB','CXY','Zab','cdx','yz[','"]~']
		print *, catmp3 /= [' !@',v3,'^&*','()_','+AB','CXY','Zab','cdx','yz[','"]~']
		print *, any(catmp3 /= [' !@',v3,'^&*','()_','+AB','CXY','Zab','cdx','yz[','"]~'])
		error stop 17
	end if

end
