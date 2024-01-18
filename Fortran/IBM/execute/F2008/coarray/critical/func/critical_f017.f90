!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : February 2011
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test critical construct with a derived type
!*                               array coarray.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	type player
		integer :: number
		character(len=10) :: name
		real(4) :: rating
	end type
	type (player), save :: team(5)[*]

	character(len=10) :: char(5) = ["alex      ", "sidney    ", "steve     ", "henrik    ", "mike      "]
	integer, parameter :: num(5) = [8, 87, 91, 33, 19]
	real(4), parameter :: score(5) = [91.5, 95.0, 95.5, 93.25, 87.75]
	integer :: n

	team(1) = player(num(1), "ovechkin", score(1))
	team(2) = player(num(2), "crosby", score(2))
	team(3) = player(num(3), "stamkos", score(3))
	team(4) = player(num(3), "sedin", score(4))
	team(5) = player(num(5), "richards", score(5))


!Part 1 Test
	sync all
	critical
		team(:)[1]%number = team(:)[1]%number - 1

		team(:)[1]%name = char
		call sleep_(1)

		do i = 1, size(team), 2
			team(:)[1]%rating = team(:)[1]%rating + 1
		end do
	end critical

	sync all
	do i = 1, 5
		if ( team(i)[1]%number /= (num(i) - num_images()) ) then
			print *, team(i)[1]%number
			print *, num(i) - num_images()
			error stop 16
		end if

		if ( team(i)[1]%name /= char(i) ) then
			print *, team(i)[1]%name
			print *, char(i)
			error stop 17
		end if

		if ( team(i)[1]%rating /= (score(i) + num_images()) ) then
			print *, team(i)[1]%rating
			print *, score(i) + num_images()
			error stop 18
		end if
	end do


!Part 2 Test
	n = 99
	team(:)%number = 0
	sync all
	do i = 1, 9999
		critical
			if (this_image() == 1) then
				team(:)%number = n
			else
				if ( team(1)%number /= team(5)%number ) then
					print *, team(1)%number, team(5)%number
					error stop 19
				end if
			end if
		end critical
	end do

end
