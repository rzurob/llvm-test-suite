!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 17, 2011
!*
!*  PRIMARY FUNCTIONS TESTED   : Test the concatenation of different length character coarray
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : No Feature Number
!*
!*  REQUIRED COMPILER OPTIONS  : -qcaf -q64
!*
!*  KEYWORD(S)                 : character, CAF
!*
!*  TARGET(S)                  : zero-length character
!*
!*  DESCRIPTION:
!*  -----------
!*  The testcase aim to
!*  1. test the concatenation of different length character coarray.
!*  2. verify that result when different length character coarray concatenate with other local character or characters on other images.
!*  -----------
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program concate002
	use char_mod
	implicit none

	integer, parameter  :: P = 1, Q = 2

	character (len=5), 		save :: coStr1[*] ! 1..9 *   5
	character (len=10), 	save :: coStr2[*] ! O/E  *  10
	character (len=100), 	save :: coStr3[*] ! 1..9 * 100

	character (len=5) 			 :: loStr1    ! O/E  *   5
 	character (len=100) 		 :: loStr2    ! 1..9 * 100

	character (len=1000) 		 :: concateStr
	character (len=1000) 		 :: verifyConcateStr
	character (len=1000), save 	 :: coConcateStr[*]

	character (len=3), save 	 :: imageIndexStr[*]
	character (len=3) 			 :: localImageIndexStr

	character (len=1), 		save :: unit_char[*], parity_char[*]
	character (len=1) 			 :: P_unit_char, P_parity_char, Q_unit_char, Q_parity_char

	integer :: me, ne, i, units

	me = this_image()
	ne = num_images()

	units = MOD(me,10)
	unit_char = ACHAR(units+48)

	if ( MOD(me,2) == 0) then
		parity_char = "E"
	else
		parity_char = "O"
	end if

	P_unit_char = '1'
	P_parity_char = 'O'
	Q_unit_char = '2'
	Q_parity_char = 'E'

	coStr1 = repeat(unit_char,5)
	coStr2 = repeat(parity_char,10)
	coStr3 = repeat(unit_char,100)

	loStr1 = repeat(parity_char,5)
	loStr2 = repeat(unit_char,100)

	verifyConcateStr = ""
	coConcateStr = ""

	sync all

		!*****************************************************!
		!******  #####  MISC CONCATENATION #####  ******!
		!******  co     // local
		!******  co[me] // local
		!******  co[P]  // local
		!******  co     // co[P]
		!******  co[me] // co[P]
		!******  co     // co
		!******  co     // co[me]
		!******  co[me] // co[me]
		!******  #####  SELF CONCATENATION #####  ******!
		!******  co     // co
		!******  co     // co[me]
		!******  co[me] // co[me]
		!*****************************************************!

	!!! ************  coarray // local  ************ !!!
	concateStr = ""
	concateStr = coStr1//loStr1 ! 5+5
	if(.NOT.( (len_trim(concateStr) == 10) .AND. (verifyChars(concateStr,1,5,unit_char)) &
	  .AND. (verifyChars(concateStr,6,10,parity_char)))) then
		error stop 11
	end if

	concateStr = trim(concateStr)//coStr3//loStr1//coStr2//loStr2 ! (5+5)+100+5+10+100
	if(.NOT.( (len_trim(concateStr) == 225) .AND. (verifyChars(concateStr,1,5,unit_char)) &
	  .AND. (verifyChars(concateStr,6,10,parity_char)) &
	  .AND. (verifyChars(concateStr,11,110,unit_char)) &
	  .AND. (verifyChars(concateStr,111,125,parity_char)) &
	  .AND. (verifyChars(concateStr,126,225,unit_char)) )) then
		error stop 12
	end if

	!!! ************  coarray[me] // local  ************ !!!
	concateStr = ""
	concateStr = coStr1[me]//loStr1 ! 5+5
	if(.NOT.( (len_trim(concateStr) == 10) .AND. (verifyChars(concateStr,1,5,unit_char)) &
	  .AND. (verifyChars(concateStr,6,10,parity_char)))) then
		error stop 13
	end if

	concateStr = trim(concateStr)//coStr3[me]//loStr1//coStr2[me]//loStr2 ! (5+5)+100+5+10+100
	if(.NOT.( (len_trim(concateStr) == 225) .AND. (verifyChars(concateStr,1,5,unit_char)) &
	  .AND. (verifyChars(concateStr,6,10,parity_char)) &
	  .AND. (verifyChars(concateStr,11,110,unit_char)) &
	  .AND. (verifyChars(concateStr,111,125,parity_char)) &
	  .AND. (verifyChars(concateStr,126,225,unit_char)) )) then
		error stop 14
	end if

	!!! ************  coarray[P] // local  ************ !!!
	concateStr = ""
	concateStr = coStr1[P]//loStr1 ! 5+5
	if(.NOT.( (len_trim(concateStr) == 10) .AND. (verifyChars(concateStr,1,5,P_unit_char)) &
	  .AND. (verifyChars(concateStr,6,10,parity_char)))) then
		error stop 15
	end if

	concateStr = trim(concateStr)//coStr3[P]//loStr1//coStr2[P]//loStr2 ! (5+5)+100+5+10+100
	if(.NOT.( (len_trim(concateStr) == 225) .AND. (verifyChars(concateStr,1,5,P_unit_char)) &
	  .AND. (verifyChars(concateStr,6,10,parity_char)) &
	  .AND. (verifyChars(concateStr,11,110,P_unit_char)) &
	  .AND. (verifyChars(concateStr,111,115,parity_char)) &
	  .AND. (verifyChars(concateStr,116,125,P_parity_char)) &
	  .AND. (verifyChars(concateStr,126,225,unit_char)) )) then
		error stop 16
	end if

	!!! ************  coarray // coarray[P]  ************ !!!
	concateStr = ""
	concateStr = coStr1//coStr2[P] ! 5+10
	if(.NOT.( (len_trim(concateStr) == 15) .AND. (verifyChars(concateStr,1,5,unit_char)) &
	  .AND. (verifyChars(concateStr,6,15,P_parity_char)))) then
		error stop 17
	end if

	concateStr = trim(concateStr)//coStr3//coStr1[P]//coStr2 ! (5+10)+100+5+10
	if(.NOT.( (len_trim(concateStr) == 130) .AND. (verifyChars(concateStr,1,5,unit_char)) &
	  .AND. (verifyChars(concateStr,6,15,P_parity_char)) &
	  .AND. (verifyChars(concateStr,16,115,unit_char)) &
	  .AND. (verifyChars(concateStr,116,120,P_unit_char)) &
	  .AND. (verifyChars(concateStr,121,130,parity_char)) )) then
		error stop 18
	end if

	!!! ************  coarray[me] // coarray[P]  ************ !!!
	concateStr = ""
	concateStr = coStr1[me]//coStr2[P] ! 5+10
	if(.NOT.( (len_trim(concateStr) == 15) .AND. (verifyChars(concateStr,1,5,unit_char)) &
	  .AND. (verifyChars(concateStr,6,15,P_parity_char)))) then
		error stop 19
	end if

	concateStr = trim(concateStr)//coStr3[me]//coStr1[P]//coStr2[me] ! (5+10)+100+5+10
	if(.NOT.( (len_trim(concateStr) == 130) .AND. (verifyChars(concateStr,1,5,unit_char)) &
	  .AND. (verifyChars(concateStr,6,15,P_parity_char)) &
	  .AND. (verifyChars(concateStr,16,115,unit_char)) &
	  .AND. (verifyChars(concateStr,116,120,P_unit_char)) &
	  .AND. (verifyChars(concateStr,121,130,parity_char)) )) then
		error stop 20
	end if

	!!! ************  coarray // coarray  ************ !!!
	concateStr = ""
	concateStr = coStr1//coStr2 ! 5+10
	if(.NOT.( (len_trim(concateStr) == 15) .AND. (verifyChars(concateStr,1,5,unit_char)) &
	  .AND. (verifyChars(concateStr,6,15,parity_char)))) then
		error stop 21
	end if

	concateStr = trim(concateStr)//coStr3//coStr2//coStr1 ! (5+10)+100+10+5
	if(.NOT.( (len_trim(concateStr) == 130) .AND. (verifyChars(concateStr,1,5,unit_char)) &
	  .AND. (verifyChars(concateStr,6,15,parity_char)) &
	  .AND. (verifyChars(concateStr,16,115,unit_char)) &
	  .AND. (verifyChars(concateStr,116,125,parity_char)) &
	  .AND. (verifyChars(concateStr,126,130,unit_char)) )) then
		error stop 22
	end if

	!!! ************  coarray // coarray[me]  ************ !!!
	concateStr = ""
	concateStr = coStr1//coStr2[me] ! 5+10
	if(.NOT.( (len_trim(concateStr) == 15) .AND. (verifyChars(concateStr,1,5,unit_char)) &
	  .AND. (verifyChars(concateStr,6,15,parity_char)))) then
		error stop 23
	end if

	concateStr = trim(concateStr)//coStr3[me]//coStr2//coStr1[me] ! (5+10)+100+10+5
	if(.NOT.( (len_trim(concateStr) == 130) .AND. (verifyChars(concateStr,1,5,unit_char)) &
	  .AND. (verifyChars(concateStr,6,15,parity_char)) &
	  .AND. (verifyChars(concateStr,16,115,unit_char)) &
	  .AND. (verifyChars(concateStr,116,125,parity_char)) &
	  .AND. (verifyChars(concateStr,126,130,unit_char)) )) then
		error stop 24
	end if

	!!! ************  coarray[me] // coarray[me]  ************ !!!
	concateStr = ""
	concateStr = coStr1[me]//coStr2[me] ! 5+10
	if(.NOT.( (len_trim(concateStr) == 15) .AND. (verifyChars(concateStr,1,5,unit_char)) &
	  .AND. (verifyChars(concateStr,6,15,parity_char)))) then
		error stop 25
	end if

	concateStr = trim(concateStr)//coStr3[me]//coStr2[me]//coStr1[me] ! (5+10)+100+10+5
	if(.NOT.( (len_trim(concateStr) == 130) .AND. (verifyChars(concateStr,1,5,unit_char)) &
	  .AND. (verifyChars(concateStr,6,15,parity_char)) &
	  .AND. (verifyChars(concateStr,16,115,unit_char)) &
	  .AND. (verifyChars(concateStr,116,125,parity_char)) &
	  .AND. (verifyChars(concateStr,126,130,unit_char)) )) then
		error stop 26
	end if

	!!! ************  SELF concatenation   ************ !!!
	concateStr = ""
	concateStr = coStr1//coStr1 ! 5+5
	if(.NOT.( (len_trim(concateStr) == 10) .AND. (verifyChars(concateStr,1,10,unit_char)))) then
		error stop 27
	end if

	concateStr = ""
	concateStr = coStr1//coStr1[me] ! 5+5
	if(.NOT.( (len_trim(concateStr) == 10) .AND. (verifyChars(concateStr,1,10,unit_char)))) then
		error stop 28
	end if

	concateStr = ""
	concateStr = coStr1[me]//coStr1[me] ! 5+5
	if(.NOT.( (len_trim(concateStr) == 10) .AND. (verifyChars(concateStr,1,10,unit_char)))) then
		error stop 29
	end if

	! ************** blend images **************!

	if (ne > MAX_IMAGE) then
		ne = MAX_IMAGE
	end if

	call getCharArray(me,imageIndexStr)
	localImageIndexStr = imageIndexStr

	sync all

	! when current image is P, concatenate all the other images
	if(me == P) then

		concateStr = ""

		do i = 1, ne
			concateStr = trim(concateStr) // coStr1[i]
		end do

		if(len_trim(concateStr) /= (ne * 5)) then
			error stop 41
		end if

		do i = 1, ne
		   verifyConcateStr((i-1)*5+1:i*5) = coStr1[i]
		end do

		if(concateStr /= verifyConcateStr) then
		   error stop 42
		end if

	! when current is not P, change P
	else if (me /= P) then

		if(me == Q) then
			coConcateStr[P] = coStr1[P]//coStr1[Q]
			if(.NOT.( (len_trim(coConcateStr[P]) == 10) .AND. (verifyChars(coConcateStr[P],1,5,P_unit_char)) &
			  .AND. (verifyChars(coConcateStr[P],6,10,Q_unit_char)) )) then
				error stop 43
			end if

			coConcateStr[P] = trim(coConcateStr[P])//coStr2[Q]//coStr2[P]

			if(.NOT.( (len_trim(coConcateStr[P]) == 30) .AND. (verifyChars(coConcateStr[P],1,5,P_unit_char)) &
			  .AND. (verifyChars(coConcateStr[P],6,10,Q_unit_char))  &
			  .AND. (verifyChars(coConcateStr[P],11,20,Q_parity_char)) &
			  .AND. (verifyChars(coConcateStr[P],21,30,P_parity_char)) )) then
				error stop 44
			end if
		end if

	end if

	sync all

	! concatenation between any two images
	do i = 1, ne
		coConcateStr = ""

		coConcateStr = coStr2//coStr2[i]

		if(len_trim(coConcateStr) /= 20) then
			error stop 45
		end if

		if(.NOT. (verifyChars(coConcateStr,1,10,parity_char))) then
			error stop 46
		end if

		if (MOD(i,2) == 0) then
			if(.NOT. (verifyChars(coConcateStr,11,20,Q_parity_char))) then
				error stop 47
			end if
		else
			if(.NOT. (verifyChars(coConcateStr,11,20,P_parity_char))) then
				error stop 48
			end if
		end if
	end do

end program concate002
