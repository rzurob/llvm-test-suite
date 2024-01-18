!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 28, 2011
!*
!*  PRIMARY FUNCTIONS TESTED   : Test the substring of different length character coarray
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : No Feature Number
!*
!*  REQUIRED COMPILER OPTIONS  : -qcaf -q64
!*
!*  KEYWORD(S)                 : character, CAF, substring
!*
!*  TARGET(S)                  : different length character
!*
!*  DESCRIPTION:
!*  -----------
!*  The testcase aim to
!*  1. test the substring of different length character coarray.
!*  -----------
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program substr001
	use char_mod
	implicit none

	integer, parameter :: P = 1, Q = 2

	character (len=10),		save :: coStr1[*]
	character (len=100), 	save :: coStr2[*]
	character (len=1000), 	save :: coStr3[*]
	character (len=2000), 	save :: coStr4[*]
	character (len=2000) 		 :: loStr

	character (len=1), save :: unit_char[*], parity_char[*]
	character (len=1) 		:: P_unit_char, P_parity_char, Q_unit_char, Q_parity_char

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

	coStr1 = repeat(unit_char,10)
	coStr2 = repeat(parity_char,100)
	coStr3 = repeat(unit_char,1000)

	sync all

		!***********************************!
		!	  test co, co[me], co[P]        !
		!	  combine with concatenation    !
		!***********************************!

	!!! ************  from beginning to end  ************ !!!

	coStr4 = ""
	coStr4 = coStr2(1:100)
	if(.NOT.( (len_trim(coStr4) == 100) .AND. (verifyChars(coStr4,1,100,parity_char)))) then
		error stop 11
	end if

	coStr4 = ""
	coStr4 = coStr2(1:100)[me]
	if(.NOT.( (len_trim(coStr4) == 100) .AND. (verifyChars(coStr4,1,100,parity_char)))) then
		error stop 12
	end if

	coStr4[me] = ""
	coStr4[me] = coStr2(1:100)[me]
	if(.NOT.( (len_trim(coStr4[me]) == 100) .AND. (verifyChars(coStr4[me],1,100,parity_char)))) then
		error stop 13
	end if

	coStr4 = ""
	coStr4 = coStr2(1:100)[P]
	if(.NOT.( (len_trim(coStr4) == 100) .AND. (verifyChars(coStr4,1,100,P_parity_char)))) then
		error stop 14
	end if

	coStr4[me] = ""
	coStr4[me] = coStr2(1:100)[P]
	if(.NOT.( (len_trim(coStr4[me]) == 100) .AND. (verifyChars(coStr4[me],1,100,P_parity_char)))) then
		error stop 15
	end if

	coStr4 = ""
	coStr4 = coStr1(1:10)
	if(.NOT.( (len_trim(coStr4) == 10) .AND. (verifyChars(coStr4,1,10,unit_char)))) then
		error stop 16
	end if

	coStr4 = ""
	coStr4 = coStr3(1:1000)
	if(.NOT.( (len_trim(coStr4) == 1000) .AND. (verifyChars(coStr4,1,1000,unit_char)))) then
		error stop 17
	end if

	coStr4 = ""
	coStr4 = coStr1(1:10) // coStr2(1:100) // coStr3(1:1000)
	if(.NOT.( (len_trim(coStr4) == 1110 ) .AND. (verifyChars(coStr4,1,10,unit_char)) &
	  .AND. (verifyChars(coStr4,11,110,parity_char)) &
	  .AND. (verifyChars(coStr4,111,1110,unit_char)))) then
		error stop 18
	end if

	coStr4[me] = ""
	coStr4[me] = coStr1(1:10)[me] // coStr2(1:100)[me] // coStr3(1:1000)[me]
	if(.NOT.( (len_trim(coStr4[me]) == 1110 ) .AND. (verifyChars(coStr4[me],1,10,unit_char)) &
	  .AND. (verifyChars(coStr4[me],11,110,parity_char)) &
	  .AND. (verifyChars(coStr4[me],111,1110,unit_char)))) then
		error stop 19
	end if

	coStr4 = ""
	coStr4 = coStr1(1:10)[P] // coStr2(1:100)[P] // coStr3(1:1000)[P]
	if(.NOT.( (len_trim(coStr4) == 1110 ) .AND. (verifyChars(coStr4,1,10,P_unit_char)) &
	  .AND. (verifyChars(coStr4,11,110,P_parity_char)) &
	  .AND. (verifyChars(coStr4,111,1110,P_unit_char)))) then
		error stop 20
	end if

	!!! ************  first character  ************ !!!

	coStr4 = ""
	coStr4(1:1) = coStr2(1:1)
	coStr4(2:2) = coStr2(1:1)[me]
	coStr4(3:3)[me] = coStr2(1:1)
	coStr4(4:4)[me] = coStr2(1:1)[me]

	coStr4(5:5) = coStr1(1:1)
	coStr4(6:6) = coStr1(1:1)[me]
	coStr4(7:7) = coStr3(1:1)
	coStr4(8:8) = coStr3(1:1)[me]

	if(.NOT.( (len_trim(coStr4) == 8 ) .AND. (verifyChars(coStr4,1,4,parity_char)) &
	  .AND. (verifyChars(coStr4,5,8,unit_char)))) then
		error stop 31
	end if

	coStr4 = ""
	coStr4(1:1) = coStr2(1:1)[P]
	coStr4(2:2)[me] = coStr2(1:1)[P]
	coStr4(3:3) = coStr1(1:1)[P]
	coStr4(4:4)[me] = coStr1(1:1)[P]
	coStr4(5:5) = coStr3(1:1)[P]
	coStr4(6:6)[me] = coStr3(1:1)[P]

	if(.NOT.( (len_trim(coStr4) == 6 ) .AND. (verifyChars(coStr4,1,2,P_parity_char)) &
	  .AND. (verifyChars(coStr4,3,6,P_unit_char)))) then
		error stop 32
	end if

	!!! ************  last character  ************ !!!

	coStr4 = ""
	coStr4(1:1) = coStr2(100:100)
	coStr4(2:2) = coStr2(100:100)[me]
	coStr4(3:3)[me] = coStr2(100:100)
	coStr4(4:4)[me] = coStr2(100:100)[me]

	coStr4(5:5) = coStr1(10:10)
	coStr4(6:6) = coStr1(10:10)[me]
	coStr4(7:7) = coStr3(1000:1000)
	coStr4(8:8) = coStr3(1000:1000)[me]

	if(.NOT.( (len_trim(coStr4) == 8 ) .AND. (verifyChars(coStr4,1,4,parity_char)) &
	  .AND. (verifyChars(coStr4,5,8,unit_char)))) then
		error stop 33
	end if

	coStr4 = ""
	coStr4(1:1) = coStr2(100:100)[P]
	coStr4(2:2)[me] = coStr2(100:100)[P]
	coStr4(3:3) = coStr1(10:10)[P]
	coStr4(4:4)[me] = coStr1(10:10)[P]
	coStr4(5:5) = coStr3(1000:1000)[P]
	coStr4(6:6)[me] = coStr3(1000:1000)[P]

	if(.NOT.( (len_trim(coStr4) == 6 ) .AND. (verifyChars(coStr4,1,2,P_parity_char)) &
	  .AND. (verifyChars(coStr4,3,6,P_unit_char)))) then
		error stop 34
	end if

	!!! ************  middle single character  ************ !!!

	coStr4 = ""
	coStr4(1:1) = coStr2(50:50)
	coStr4(2:2) = coStr2(50:50)[me]
	coStr4(3:3)[me] = coStr2(50:50)
	coStr4(4:4)[me] = coStr2(50:50)[me]

	coStr4(5:5) = coStr1(5:5)
	coStr4(6:6) = coStr1(5:5)[me]
	coStr4(7:7) = coStr3(500:500)
	coStr4(8:8) = coStr3(500:500)[me]

	if(.NOT.( (len_trim(coStr4) == 8 ) .AND. (verifyChars(coStr4,1,4,parity_char)) &
	  .AND. (verifyChars(coStr4,5,8,unit_char)))) then
		error stop 35
	end if

	coStr4 = ""
	coStr4(1:1) = coStr2(50:50)[P]
	coStr4(2:2)[me] = coStr2(50:50)[P]
	coStr4(3:3) = coStr1(5:5)[P]
	coStr4(4:4)[me] = coStr1(5:5)[P]
	coStr4(5:5) = coStr3(500:500)[P]
	coStr4(6:6)[me] = coStr3(500:500)[P]

	if(.NOT.( (len_trim(coStr4) == 6 ) .AND. (verifyChars(coStr4,1,2,P_parity_char)) &
	  .AND. (verifyChars(coStr4,3,6,P_unit_char)))) then
		error stop 36
	end if

	!!! ************  middle many characters   ************ !!!

	coStr4 = ""
	coStr4(1:5) = coStr2(48:52)
	coStr4(6:10) = coStr2(48:52)[me]
	coStr4(11:15)[me] = coStr2(48:52)
	coStr4(16:20)[me] = coStr2(48:52)[me]

	coStr4(21:25) = coStr1(3:7)
	coStr4(26:30) = coStr1(3:7)[me]
	coStr4(31:35) = coStr3(498:502)
	coStr4(36:40) = coStr3(498:502)[me]

	if(.NOT.( (len_trim(coStr4) == 40 ) .AND. (verifyChars(coStr4,1,20,parity_char)) &
	  .AND. (verifyChars(coStr4,21,40,unit_char)))) then
		error stop 37
	end if

	coStr4 = ""
	coStr4(1:5) = coStr2(48:52)[P]
	coStr4(6:10)[me] = coStr2(48:52)[P]
	coStr4(11:15) = coStr1(3:7)[P]
	coStr4(16:20)[me] = coStr1(3:7)[P]
	coStr4(21:25) = coStr3(498:502)[P]
	coStr4(26:30)[me] = coStr3(498:502)[P]

	if(.NOT.( (len_trim(coStr4) == 30 ) .AND. (verifyChars(coStr4,1,10,P_parity_char)) &
	  .AND. (verifyChars(coStr4,11,30,P_unit_char)))) then
		error stop 38
	end if

	!!! ************  some string including the first character   ************ !!!

	coStr4 = ""
	coStr4(1:5) = coStr2(1:5)
	coStr4(6:10) = coStr2(1:5)[me]
	coStr4(11:15)[me] = coStr2(1:5)
	coStr4(16:20)[me] = coStr2(1:5)[me]

	coStr4(21:25) = coStr1(1:5)
	coStr4(26:30) = coStr1(1:5)[me]
	coStr4(31:35) = coStr3(1:5)
	coStr4(36:40) = coStr3(1:5)[me]

	if(.NOT.( (len_trim(coStr4) == 40 ) .AND. (verifyChars(coStr4,1,20,parity_char)) &
	  .AND. (verifyChars(coStr4,21,40,unit_char)))) then
		error stop 39
	end if

	coStr4 = ""
	coStr4(1:5) = coStr2(1:5)[P]
	coStr4(6:10)[me] = coStr2(1:5)[P]
	coStr4(11:15) = coStr1(1:5)[P]
	coStr4(16:20)[me] = coStr1(1:5)[P]
	coStr4(21:25) = coStr3(1:5)[P]
	coStr4(26:30)[me] = coStr3(1:5)[P]

	if(.NOT.( (len_trim(coStr4) == 30 ) .AND. (verifyChars(coStr4,1,10,P_parity_char)) &
	  .AND. (verifyChars(coStr4,11,30,P_unit_char)))) then
		error stop 40
	end if

	!!! ************  some string including the last character   ************ !!!

	coStr4 = ""
	coStr4(1:5) = coStr2(96:100)
	coStr4(6:10) = coStr2(96:100)[me]
	coStr4(11:15)[me] = coStr2(96:100)
	coStr4(16:20)[me] = coStr2(96:100)[me]

	coStr4(21:25) = coStr1(6:10)
	coStr4(26:30) = coStr1(6:10)[me]
	coStr4(31:35) = coStr3(996:1000)
	coStr4(36:40) = coStr3(996:1000)[me]

	if(.NOT.( (len_trim(coStr4) == 40 ) .AND. (verifyChars(coStr4,1,20,parity_char)) &
	  .AND. (verifyChars(coStr4,21,40,unit_char)))) then
		error stop 41
	end if

	coStr4 = ""
	coStr4(1:5) = coStr2(96:100)[P]
	coStr4(6:10)[me] = coStr2(96:100)[P]
	coStr4(11:15) = coStr1(6:10)[P]
	coStr4(16:20)[me] = coStr1(6:10)[P]
	coStr4(21:25) = coStr3(996:1000)[P]
	coStr4(26:30)[me] = coStr3(996:1000)[P]

	if(.NOT.( (len_trim(coStr4) == 30 ) .AND. (verifyChars(coStr4,1,10,P_parity_char)) &
	  .AND. (verifyChars(coStr4,11,30,P_unit_char)))) then
		error stop 42
	end if

	! ************** blend images ************** !

	if (ne > MAX_IMAGE) then
		ne = MAX_IMAGE
	end if

	sync all

	! when current image is P, compare the results from concatenation and substring
	if(me == P) then

		coStr4 = ""

		do i = 1, ne
			coStr4 = trim(coStr4) // coStr1(1:5)[i]
		end do

		if(len_trim(coStr4) /= (ne * 5)) then
			error stop 51
		end if

		do i = 1, ne
		   loStr((i-1)*5+1:i*5) = coStr1(6:10)[i]
		end do

		if(coStr4 /= loStr) then
		   error stop 52
		end if


	else if (me /= P) then
		! when current image is Q, substring from P & Q, then change P
		if(me == Q) then
			coStr4[P] = ""
			coStr4[P] = coStr1(1:5)[P] // coStr1(6:10)[Q]
			if(.NOT.( (len_trim(coStr4[P]) == 10) .AND. (verifyChars(coStr4[P],1,5,P_unit_char)) &
			  .AND. (verifyChars(coStr4[P],6,10,Q_unit_char)) )) then
				error stop 53
			end if

			coStr4[P] = ""
			coStr4[P] = coStr1(3:7)[P] // coStr1(3:7)[Q]
			if(.NOT.( (len_trim(coStr4[P]) == 10) .AND. (verifyChars(coStr4[P],1,5,P_unit_char)) &
			  .AND. (verifyChars(coStr4[P],6,10,Q_unit_char)) )) then
				error stop 54
			end if

			coStr4[P] = ""
			coStr4[P] = coStr1(6:10)[P] // coStr1(1:5)[Q]
			if(.NOT.( (len_trim(coStr4[P]) == 10) .AND. (verifyChars(coStr4[P],1,5,P_unit_char)) &
			  .AND. (verifyChars(coStr4[P],6,10,Q_unit_char)) )) then
				error stop 55
			end if
		end if

	end if

	sync all

	! each image substring from each of the others, then verify the results
	do i = 1, ne
		coStr4[me] = ""
		coStr4[me] = coStr3(i:2*i)[i]
		if(.NOT. (verifyChars(coStr4,1,i,unit_char[i]))) then
			error stop 61
		end if
	end do

end program substr001
