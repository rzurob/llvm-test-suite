!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : October 2010
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test codimension attribute vs entity declaration for a static coarray.
!*
!234567890123456789012345678901234567890123456789012345678901234567890


	real*8, save, codimension[2,*] :: caf1[*]
	integer*8, save, codimension[*] :: caf2(2,2)[10:*]
	complex*8, save, codimension[2,2,*] :: caf3[0:1,1:2,2:*]
	logical*8, save, dimension(3), codimension[1:2,0:*] :: caf4[5,4,3,2,*]

	type blk
		integer :: x
		logical :: flag
	end type
	type (blk) :: dts

	integer, allocatable :: a(:), b(:)
	integer :: num
	num = num_images()
	dts%flag = .false.
	dts%x = -1


!%%%%%%%%%%%%%%%%%%%%%%% CAF1 %%%%%%%%%%%%%%%%%%%%%%%
	!#### Test coarray bounds
	a = lcobound(caf1)
	b = ucobound(caf1)

	if ((size(a) /= 1) .or. (size(b) /= 1)) then
		error stop 11
	end if
	if ( any(a .ne. [1]) ) then
		print *, a
		error stop 12
	end if
	if ( any(b .ne. [num]) ) then
		print *, b, num
		error stop 13
	end if

	!#### Test coarray storage
	caf1 = dts%x
	caf1[1] = dts%x + 1
	if (this_image() == 1) then
		if (caf1 /= 0) then
			print *, caf1
			error stop 14
		end if
	else
		if (caf1 /= -1) then
			print *, caf1
			error stop 15
		end if
	end if


!%%%%%%%%%%%%%%%%%%%%%%% CAF2 %%%%%%%%%%%%%%%%%%%%%%%
	!#### Test coarray bounds
	a = lcobound(caf2)
	b = ucobound(caf2)

	if ((size(a) /= 1) .or. (size(b) /= 1)) then
		error stop 21
	end if
	if ( any(a .ne. [10]) ) then
		print *, a
		error stop 22
	end if
	if ( any(b .ne. [9 + num]) ) then
		print *, b, 9 + num
		error stop 23
	end if

	!#### Test coarray storage
	caf2 = caf1
	if (this_image() == 1) then
		if ( any(caf2 .ne. reshape([0,0,0,0], [2,2])) ) then
			print *, caf2
			error stop 24
		end if
	else
		if ( any(caf2 .ne. reshape([-1,-1,-1,-1], [2,2])) ) then
			print *, caf2
			error stop 25
		end if
	end if


!%%%%%%%%%%%%%%%%%%%%%%% CAF3 %%%%%%%%%%%%%%%%%%%%%%%
	!#### Test coarray bounds
	a = lcobound(caf3)
	b = ucobound(caf3)

	if ((size(a) /= 3) .or. (size(b) /= 3)) then
		error stop 31
	end if
	if ( any(a .ne. [0,1,2]) ) then
		print *, a
		error stop 32
	end if
	if ( any(b .ne. [1,2,3]) ) then
		print *, b
		error stop 33
	end if

	!#### Test coarray storage
	caf3 = (12.5, -15.25)
	if (caf3 /= (12.5, -15.25)) then
		print *, caf3
		error stop 34
	end if


!%%%%%%%%%%%%%%%%%%%%%%% CAF4 %%%%%%%%%%%%%%%%%%%%%%%
	!#### Test coarray bounds
	a = lcobound(caf4)
	b = ucobound(caf4)

	if ((size(a) /= 5) .or. (size(b) /= 5)) then
		error stop 41
	end if
	if ( any(a .ne. [1,1,1,1,1]) ) then
		print *, a
		error stop 42
	end if
	if ( any(b .ne. [5,4,3,2,1]) ) then
		print *, b
		error stop 43
	end if

	!#### Test coarray storage
	caf4 = dts%flag
	caf4[1,1,1,1,1] = .true.
	caf4[2,1,1,1,1] = .true.
	if (this_image() == 1) then
		if (.not. caf4(1)) then
			print *, caf4
			error stop 44
		end if
	else if (this_image() == 2) then
		if (.not. caf4(2)) then
			print *, caf4
			error stop 45
		end if
	else
		if (caf4(3)) then
			print *, caf4
			error stop 46
		end if
	end if

end
