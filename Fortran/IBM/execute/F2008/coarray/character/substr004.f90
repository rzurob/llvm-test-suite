!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 28, 2011
!*
!*  PRIMARY FUNCTIONS TESTED   : Test the substring of character component of derived type coarray
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : No Feature Number
!*
!*  REQUIRED COMPILER OPTIONS  : -qcaf -q64
!*
!*  KEYWORD(S)                 : character, CAF, substring
!*
!*  TARGET(S)                  : character component of derived type
!*
!*  DESCRIPTION:
!*  -----------
!*  The testcase aim to
!*  1. test the substring of character component of derived type coarray
!*  -----------
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program substr004
	use char_mod
	implicit none

	integer, parameter :: P = 1, Q = 2

	type co_dt_type
		character (len=10) 	 	:: coStr1
		character (len=100)  	:: coStr2
		character (len=1000)	:: coStr3
		character (len=2000) 	:: coStr4
	end type

	type (co_dt_type), save :: co_dt[*]
	character (len=2000) 	:: loStr

	character (len=1), save :: unit_char[*], parity_char[*]
	character (len=1) :: P_unit_char, P_parity_char, Q_unit_char, Q_parity_char

	integer :: me, ne, i, units

	me = this_image()
	ne = num_images()

	units = MOD(me,10)
	unit_char = ACHAR(units+48)

	if (MOD(me,2) == 0) then
		parity_char = "E"
	else
		parity_char = "O"
	end if

	P_unit_char 	= '1'
	P_parity_char 	= 'O'
	Q_unit_char 	= '2'
	Q_parity_char 	= 'E'

	co_dt%coStr1 = repeat(unit_char,10)
	co_dt%coStr2 = repeat(parity_char,100)
	co_dt%coStr3 = repeat(unit_char,1000)

	sync all

	!!! ************  from beginning to end  ************ !!!

	co_dt%coStr4 = ""
	co_dt%coStr4 = co_dt%coStr2(1:100)
	!print *, co_dt%coStr2(1:100)
	!print *, co_dt%coStr4, co_dt%coStr2(1:100)
	if(.NOT.( (len_trim(co_dt%coStr4) == 100) .AND. (verifyChars(co_dt%coStr4,1,100,parity_char)))) then
	!    print *, (len_trim(co_dt%coStr4) == 100), verifyChars(co_dt%coStr4,1,100,parity_char)
		error stop 11
	end if

	co_dt%coStr4 = ""
	co_dt%coStr4 = co_dt[me]%coStr2(1:100)
	if(.NOT.( (len_trim(co_dt%coStr4) == 100) .AND. (verifyChars(co_dt%coStr4,1,100,parity_char)))) then
		error stop 12
	end if

	co_dt[me]%coStr4 = ""
	co_dt[me]%coStr4 = co_dt[me]%coStr2(1:100)
	if(.NOT.( (len_trim(co_dt[me]%coStr4) == 100) .AND. (verifyChars(co_dt[me]%coStr4,1,100,parity_char)))) then
		error stop 13
	end if

	co_dt%coStr4 = ""
	co_dt%coStr4 = co_dt[P]%coStr2(1:100)
	if(.NOT.( (len_trim(co_dt%coStr4) == 100) .AND. (verifyChars(co_dt%coStr4,1,100,P_parity_char)))) then
		error stop 14
	end if

	co_dt[me]%coStr4 = ""
	co_dt[me]%coStr4 = co_dt[P]%coStr2(1:100)
	if(.NOT.( (len_trim(co_dt[me]%coStr4) == 100) .AND. (verifyChars(co_dt[me]%coStr4,1,100,P_parity_char)))) then
		error stop 15
	end if

	co_dt%coStr4 = ""
	co_dt%coStr4 = co_dt%coStr1(1:10)
	if(.NOT.( (len_trim(co_dt%coStr4) == 10) .AND. (verifyChars(co_dt%coStr4,1,10,unit_char)))) then
		error stop 16
	end if

	co_dt%coStr4 = ""
	co_dt%coStr4 = co_dt%coStr3(1:1000)
	if(.NOT.( (len_trim(co_dt%coStr4) == 1000) .AND. (verifyChars(co_dt%coStr4,1,1000,unit_char)))) then
		error stop 17
	end if

	co_dt%coStr4 = ""
	co_dt%coStr4 = co_dt%coStr1(1:10) // co_dt%coStr2(1:100) // co_dt%coStr3(1:1000)
	if(.NOT.( (len_trim(co_dt%coStr4) == 1110 ) .AND. (verifyChars(co_dt%coStr4,1,10,unit_char)) &
	  .AND. (verifyChars(co_dt%coStr4,11,110,parity_char)) &
	  .AND. (verifyChars(co_dt%coStr4,111,1110,unit_char)))) then
		error stop 18
	end if

	co_dt[me]%coStr4 = ""
	co_dt[me]%coStr4 = co_dt[me]%coStr1(1:10) // co_dt[me]%coStr2(1:100) // co_dt[me]%coStr3(1:1000)
	if(.NOT.( (len_trim(co_dt[me]%coStr4) == 1110 ) .AND. (verifyChars(co_dt[me]%coStr4,1,10,unit_char)) &
	  .AND. (verifyChars(co_dt[me]%coStr4,11,110,parity_char)) &
	  .AND. (verifyChars(co_dt[me]%coStr4,111,1110,unit_char)))) then
		error stop 19
	end if

	co_dt%coStr4 = ""
	co_dt%coStr4 = co_dt[P]%coStr1(1:10) // co_dt[P]%coStr2(1:100) // co_dt[P]%coStr3(1:1000)
	if(.NOT.( (len_trim(co_dt%coStr4) == 1110 ) .AND. (verifyChars(co_dt%coStr4,1,10,P_unit_char)) &
	  .AND. (verifyChars(co_dt%coStr4,11,110,P_parity_char)) &
	  .AND. (verifyChars(co_dt%coStr4,111,1110,P_unit_char)))) then
		error stop 20
	end if

	!!! ************  first character  ************ !!!

	co_dt%coStr4 = ""
	co_dt%coStr4(1:1) = co_dt%coStr2(1:1)
	co_dt%coStr4(2:2) = co_dt[me]%coStr2(1:1)
	co_dt[me]%coStr4(3:3) = co_dt%coStr2(1:1)
	co_dt[me]%coStr4(4:4) = co_dt[me]%coStr2(1:1)

	co_dt%coStr4(5:5) = co_dt%coStr1(1:1)
	co_dt%coStr4(6:6) = co_dt[me]%coStr1(1:1)
	co_dt%coStr4(7:7) = co_dt%coStr3(1:1)
	co_dt%coStr4(8:8) = co_dt[me]%coStr3(1:1)

	if(.NOT.( (len_trim(co_dt%coStr4) == 8 ) .AND. (verifyChars(co_dt%coStr4,1,4,parity_char)) &
	  .AND. (verifyChars(co_dt%coStr4,5,8,unit_char)))) then
		error stop 31
	end if

	co_dt%coStr4 = ""
	co_dt%coStr4(1:1) = co_dt[P]%coStr2(1:1)
	co_dt[me]%coStr4(2:2) = co_dt[P]%coStr2(1:1)
	co_dt%coStr4(3:3) = co_dt[P]%coStr1(1:1)
	co_dt[me]%coStr4(4:4) = co_dt[P]%coStr1(1:1)
	co_dt%coStr4(5:5) = co_dt[P]%coStr3(1:1)
	co_dt[me]%coStr4(6:6) = co_dt[P]%coStr3(1:1)

	if(.NOT.( (len_trim(co_dt%coStr4) == 6 ) .AND. (verifyChars(co_dt%coStr4,1,2,P_parity_char)) &
	  .AND. (verifyChars(co_dt%coStr4,3,6,P_unit_char)))) then
		error stop 32
	end if

	!!! ************  last character  ************ !!!

	co_dt%coStr4 = ""
	co_dt%coStr4(1:1) = co_dt%coStr2(100:100)
	co_dt%coStr4(2:2) = co_dt[me]%coStr2(100:100)
	co_dt[me]%coStr4(3:3) = co_dt%coStr2(100:100)
	co_dt[me]%coStr4(4:4) = co_dt[me]%coStr2(100:100)

	co_dt%coStr4(5:5) = co_dt%coStr1(10:10)
	co_dt%coStr4(6:6) = co_dt[me]%coStr1(10:10)
	co_dt%coStr4(7:7) = co_dt%coStr3(1000:1000)
	co_dt%coStr4(8:8) = co_dt[me]%coStr3(1000:1000)

	if(.NOT.( (len_trim(co_dt%coStr4) == 8 ) .AND. (verifyChars(co_dt%coStr4,1,4,parity_char)) &
	  .AND. (verifyChars(co_dt%coStr4,5,8,unit_char)))) then
		error stop 33
	end if

	co_dt%coStr4 = ""
	co_dt%coStr4(1:1) = co_dt[P]%coStr2(100:100)
	co_dt[me]%coStr4(2:2) = co_dt[P]%coStr2(100:100)
	co_dt%coStr4(3:3) = co_dt[P]%coStr1(10:10)
	co_dt[me]%coStr4(4:4) = co_dt[P]%coStr1(10:10)
	co_dt%coStr4(5:5) = co_dt[P]%coStr3(1000:1000)
	co_dt[me]%coStr4(6:6) = co_dt[P]%coStr3(1000:1000)

	if(.NOT.( (len_trim(co_dt%coStr4) == 6 ) .AND. (verifyChars(co_dt%coStr4,1,2,P_parity_char)) &
	  .AND. (verifyChars(co_dt%coStr4,3,6,P_unit_char)))) then
		error stop 34
	end if

	!!! ************  middle single character  ************ !!!

	co_dt%coStr4 = ""
	co_dt%coStr4(1:1) = co_dt%coStr2(50:50)
	co_dt%coStr4(2:2) = co_dt[me]%coStr2(50:50)
	co_dt[me]%coStr4(3:3) = co_dt%coStr2(50:50)
	co_dt[me]%coStr4(4:4) = co_dt[me]%coStr2(50:50)

	co_dt%coStr4(5:5) = co_dt%coStr1(5:5)
	co_dt%coStr4(6:6) = co_dt[me]%coStr1(5:5)
	co_dt%coStr4(7:7) = co_dt%coStr3(500:500)
	co_dt%coStr4(8:8) = co_dt[me]%coStr3(500:500)

	if(.NOT.( (len_trim(co_dt%coStr4) == 8 ) .AND. (verifyChars(co_dt%coStr4,1,4,parity_char)) &
	  .AND. (verifyChars(co_dt%coStr4,5,8,unit_char)))) then
		error stop 35
	end if

	co_dt%coStr4 = ""
	co_dt%coStr4(1:1) = co_dt[P]%coStr2(50:50)
	co_dt[me]%coStr4(2:2) = co_dt[P]%coStr2(50:50)
	co_dt%coStr4(3:3) = co_dt[P]%coStr1(5:5)
	co_dt[me]%coStr4(4:4) = co_dt[P]%coStr1(5:5)
	co_dt%coStr4(5:5) = co_dt[P]%coStr3(500:500)
	co_dt[me]%coStr4(6:6) = co_dt[P]%coStr3(500:500)

	if(.NOT.( (len_trim(co_dt%coStr4) == 6 ) .AND. (verifyChars(co_dt%coStr4,1,2,P_parity_char)) &
	  .AND. (verifyChars(co_dt%coStr4,3,6,P_unit_char)))) then
		error stop 36
	end if

	!!! ************  middle many characters   ************ !!!

	co_dt%coStr4 = ""
	co_dt%coStr4(1:5) = co_dt%coStr2(48:52)
	co_dt%coStr4(6:10) = co_dt[me]%coStr2(48:52)
	co_dt[me]%coStr4(11:15) = co_dt%coStr2(48:52)
	co_dt[me]%coStr4(16:20) = co_dt[me]%coStr2(48:52)

	co_dt%coStr4(21:25) = co_dt%coStr1(3:7)
	co_dt%coStr4(26:30) = co_dt[me]%coStr1(3:7)
	co_dt%coStr4(31:35) = co_dt%coStr3(498:502)
	co_dt%coStr4(36:40) = co_dt[me]%coStr3(498:502)

	if(.NOT.( (len_trim(co_dt%coStr4) == 40 ) .AND. (verifyChars(co_dt%coStr4,1,20,parity_char)) &
	  .AND. (verifyChars(co_dt%coStr4,21,40,unit_char)))) then
		error stop 37
	end if

	co_dt%coStr4 = ""
	co_dt%coStr4(1:5) = co_dt[P]%coStr2(48:52)
	co_dt[me]%coStr4(6:10) = co_dt[P]%coStr2(48:52)
	co_dt%coStr4(11:15) = co_dt[P]%coStr1(3:7)
	co_dt[me]%coStr4(16:20) = co_dt[P]%coStr1(3:7)
	co_dt%coStr4(21:25) = co_dt[P]%coStr3(498:502)
	co_dt[me]%coStr4(26:30) = co_dt[P]%coStr3(498:502)

	if(.NOT.( (len_trim(co_dt%coStr4) == 30 ) .AND. (verifyChars(co_dt%coStr4,1,10,P_parity_char)) &
	  .AND. (verifyChars(co_dt%coStr4,11,30,P_unit_char)))) then
		error stop 38
	end if

	!!! ************  some string including the first character   ************ !!!

	co_dt%coStr4 = ""
	co_dt%coStr4(1:5) = co_dt%coStr2(1:5)
	co_dt%coStr4(6:10) = co_dt[me]%coStr2(1:5)
	co_dt[me]%coStr4(11:15) = co_dt%coStr2(1:5)
	co_dt[me]%coStr4(16:20) = co_dt[me]%coStr2(1:5)

	co_dt%coStr4(21:25) = co_dt%coStr1(1:5)
	co_dt%coStr4(26:30) = co_dt[me]%coStr1(1:5)
	co_dt%coStr4(31:35) = co_dt%coStr3(1:5)
	co_dt%coStr4(36:40) = co_dt[me]%coStr3(1:5)

	if(.NOT.( (len_trim(co_dt%coStr4) == 40 ) .AND. (verifyChars(co_dt%coStr4,1,20,parity_char)) &
	  .AND. (verifyChars(co_dt%coStr4,21,40,unit_char)))) then
		error stop 39
	end if

	co_dt%coStr4 = ""
	co_dt%coStr4(1:5) = co_dt[P]%coStr2(1:5)
	co_dt[me]%coStr4(6:10) = co_dt[P]%coStr2(1:5)
	co_dt%coStr4(11:15) = co_dt[P]%coStr1(1:5)
	co_dt[me]%coStr4(16:20) = co_dt[P]%coStr1(1:5)
	co_dt%coStr4(21:25) = co_dt[P]%coStr3(1:5)
	co_dt[me]%coStr4(26:30) = co_dt[P]%coStr3(1:5)

	if(.NOT.( (len_trim(co_dt%coStr4) == 30 ) .AND. (verifyChars(co_dt%coStr4,1,10,P_parity_char)) &
	  .AND. (verifyChars(co_dt%coStr4,11,30,P_unit_char)))) then
		error stop 40
	end if

	!!! ************  some string including the last character   ************ !!!

	co_dt%coStr4 = ""
	co_dt%coStr4(1:5) = co_dt%coStr2(96:100)
	co_dt%coStr4(6:10) = co_dt[me]%coStr2(96:100)
	co_dt[me]%coStr4(11:15) = co_dt%coStr2(96:100)
	co_dt[me]%coStr4(16:20) = co_dt[me]%coStr2(96:100)

	co_dt%coStr4(21:25) = co_dt%coStr1(6:10)
	co_dt%coStr4(26:30) = co_dt[me]%coStr1(6:10)
	co_dt%coStr4(31:35) = co_dt%coStr3(996:1000)
	co_dt%coStr4(36:40) = co_dt[me]%coStr3(996:1000)

	if(.NOT.( (len_trim(co_dt%coStr4) == 40 ) .AND. (verifyChars(co_dt%coStr4,1,20,parity_char)) &
	  .AND. (verifyChars(co_dt%coStr4,21,40,unit_char)))) then
		error stop 41
	end if

	co_dt%coStr4 = ""
	co_dt%coStr4(1:5) = co_dt[P]%coStr2(96:100)
	co_dt[me]%coStr4(6:10) = co_dt[P]%coStr2(96:100)
	co_dt%coStr4(11:15) = co_dt[P]%coStr1(6:10)
	co_dt[me]%coStr4(16:20) = co_dt[P]%coStr1(6:10)
	co_dt%coStr4(21:25) = co_dt[P]%coStr3(996:1000)
	co_dt[me]%coStr4(26:30) = co_dt[P]%coStr3(996:1000)

	if(.NOT.( (len_trim(co_dt%coStr4) == 30 ) .AND. (verifyChars(co_dt%coStr4,1,10,P_parity_char)) &
	  .AND. (verifyChars(co_dt%coStr4,11,30,P_unit_char)))) then
		error stop 42
	end if

	! ************** blend images ************** !

	if (ne > MAX_IMAGE) then
		ne = MAX_IMAGE
	end if

	sync all

	! when current image is P, compare the results from concatenation and substring
	if(me == P) then

		co_dt%coStr4 = ""

		do i = 1, ne
			co_dt%coStr4 = trim(co_dt[i]%coStr4) // co_dt%coStr1(1:5)
		end do

		if(len_trim(co_dt%coStr4) /= (ne * 5)) then
			error stop 51
		end if

		do i = 1, ne
		   loStr((i-1)*5+1:i*5) = co_dt[i]%coStr1(6:10)
		end do

		if(co_dt%coStr4 /= loStr) then
		   error stop 52
		end if

	else if (me /= P) then ! me == Q, change P
		! when current image is Q, substring from P & Q, then change P
		if(me == Q) then
			co_dt[P]%coStr4 = ""
			co_dt[P]%coStr4 = co_dt[P]%coStr1(1:5) // co_dt[Q]%coStr1(6:10)
			if(.NOT.( (len_trim(co_dt[P]%coStr4) == 10) .AND. (verifyChars(co_dt[P]%coStr4,1,5,P_unit_char)) &
			  .AND. (verifyChars(co_dt[P]%coStr4,6,10,Q_unit_char)) )) then
				error stop 53
			end if

			co_dt[P]%coStr4 = ""
			co_dt[P]%coStr4 = co_dt[P]%coStr1(3:7) // co_dt[Q]%coStr1(3:7)
			if(.NOT.( (len_trim(co_dt[P]%coStr4) == 10) .AND. (verifyChars(co_dt[P]%coStr4,1,5,P_unit_char)) &
			  .AND. (verifyChars(co_dt[P]%coStr4,6,10,Q_unit_char)) )) then
				error stop 54
			end if

			co_dt[P]%coStr4 = ""
			co_dt[P]%coStr4 = co_dt[P]%coStr1(6:10) // co_dt[Q]%coStr1(1:5)
			if(.NOT.( (len_trim(co_dt[P]%coStr4) == 10) .AND. (verifyChars(co_dt[P]%coStr4,1,5,P_unit_char)) &
			  .AND. (verifyChars(co_dt[P]%coStr4,6,10,Q_unit_char)) )) then
				error stop 55
			end if
		end if

	end if

	sync all

	! each image substring from each of the others, then verify the results
	do i = 1, ne
		co_dt[me]%coStr4 = ""
		co_dt[me]%coStr4 = co_dt[i]%coStr3(i:2*i)
		if(.NOT. (verifyChars(co_dt[i]%coStr4,1,i,unit_char))) then
			error stop 61
		end if
	end do

end program substr004
