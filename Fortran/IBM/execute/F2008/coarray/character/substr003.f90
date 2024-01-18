!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : substr003.f
!*  TEST CASE TITLE            : Test the substring of character array coarray
!*                               
!*  PROGRAMMER                 : Ke Wen Lin 
!*  DATE                       : March 28, 2011 
!*  ORIGIN                     : Compiler Development, IBM CDL
!*
!*  PRIMARY FUNCTIONS TESTED   : Test the substring of character array coarray
!*                              
!*  SECONDARY FUNCTIONS TESTED :                                                      
!*  
!*  REFERENCE                  : No Feature Number
!*
!*  DRIVER STANZA              : xlf2003_r
!*  REQUIRED COMPILER OPTIONS  : -qcaf -q64
!*
!*  KEYWORD(S)                 : character, CAF, substring, array
!* 
!*  TARGET(S)                  : character array coarray           
!*
!*  DESCRIPTION:
!*  -----------
!*  The testcase aim to 
!*  1. test the substring of character array coarray
!*  -----------
!*  
!234567890123456789012345678901234567890123456789012345678901234567890

program substr003
	use char_mod
	implicit none
	
	integer, parameter :: P = 1, Q = 2
	
	character (len=100), 	save :: coStr(3)[-1:0,0:1,2:*] 
	character (len=2000),	save :: coStr4[0:2,1:*] 
	character (len=1), 		save :: unit_char[*], parity_char[*]

	character (len=2000) :: loStr 
	character (len=1) 	 :: P_unit_char, P_parity_char, Q_unit_char, Q_parity_char

	integer :: coStr_low_bounds(3), coStr_up_bounds(3), coStr_index(3)
	integer :: coStr4_low_bounds(2), coStr4_up_bounds(2), coStr4_index(2)

	integer :: me, ne, i, units, j, k, m

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

	coStr(1) = repeat(unit_char,100)
	coStr(2) = repeat(parity_char,100)
	coStr(3) = repeat(unit_char,100)

	coStr_low_bounds = lcobound(coStr)
	coStr_up_bounds = ucobound(coStr)

	coStr4_low_bounds = lcobound(coStr4)
	coStr4_low_bounds = ucobound(coStr4)

	coStr_index = this_image(coStr)
	coStr4_index = this_image(coStr4)

	sync all
	
	!!! ************  from beginning to end  ************ !!!

	coStr4 = ""
	coStr4 = coStr(2)(1:100)
	if(.NOT.( (len_trim(coStr4) == 100) .AND. (verifyChars(coStr4,1,100,parity_char)))) then
		error stop 11
	end if

	coStr4 = ""
	coStr4 = coStr(2)(1:100)[coStr_index(1),coStr_index(2),coStr_index(3)]
	if(.NOT.( (len_trim(coStr4) == 100) .AND. (verifyChars(coStr4,1,100,parity_char)))) then
		error stop 12
	end if

	coStr4[coStr4_index(1),coStr4_index(2)] = ""
	coStr4[coStr4_index(1),coStr4_index(2)] = coStr(2)(1:100)[coStr_index(1),coStr_index(2),coStr_index(3)]
	if(.NOT.( (len_trim(coStr4[coStr4_index(1),coStr4_index(2)]) == 100) .AND. (verifyChars(coStr4[coStr4_index(1),coStr4_index(2)],1,100,parity_char)))) then
		error stop 13
	end if

	coStr4 = ""
	coStr4 = coStr(2)(1:100)[-1,0,2]
	if(.NOT.( (len_trim(coStr4) == 100) .AND. (verifyChars(coStr4,1,100,P_parity_char)))) then
		error stop 14
	end if

	coStr4[coStr4_index(1),coStr4_index(2)] = ""
	coStr4[coStr4_index(1),coStr4_index(2)] = coStr(2)(1:100)[-1,0,2]
	if(.NOT.( (len_trim(coStr4[coStr4_index(1),coStr4_index(2)]) == 100) .AND. (verifyChars(coStr4[coStr4_index(1),coStr4_index(2)],1,100,P_parity_char)))) then
		error stop 15
	end if

	coStr4 = ""
	coStr4 = coStr(1)(1:1) // coStr(2)(1:10) // coStr(3)(1:100)
	if(.NOT.( (len_trim(coStr4) == 111 ) .AND. (verifyChars(coStr4,1,1,unit_char)) &
	  .AND. (verifyChars(coStr4,2,11,parity_char)) &
	  .AND. (verifyChars(coStr4,12,111,unit_char)))) then
		error stop 16
	end if 

	coStr4[coStr4_index(1),coStr4_index(2)] = ""
	coStr4[coStr4_index(1),coStr4_index(2)] = coStr(1)(1:1)[coStr_index(1),coStr_index(2),coStr_index(3)] // coStr(2)(1:10)[coStr_index(1),coStr_index(2),coStr_index(3)] // coStr(3)(1:100)[coStr_index(1),coStr_index(2),coStr_index(3)]
	if(.NOT.( (len_trim(coStr4) == 111 ) .AND. (verifyChars(coStr4,1,1,unit_char)) &
	  .AND. (verifyChars(coStr4,2,11,parity_char)) &
	  .AND. (verifyChars(coStr4,12,111,unit_char)))) then
		error stop 17
	end if

	coStr4 = ""
	coStr4 = coStr(1)(1:1)[-1,0,2] // coStr(2)(1:10)[-1,0,2] // coStr(3)(1:100)[-1,0,2]
	if(.NOT.( (len_trim(coStr4) == 111 ) .AND. (verifyChars(coStr4,1,1,P_unit_char)) &
	  .AND. (verifyChars(coStr4,2,11,P_parity_char)) &
	  .AND. (verifyChars(coStr4,12,111,P_unit_char)))) then
		error stop 18
	end if

	!!! ************  first character  ************ !!!

	coStr4 = ""
	coStr4(1:1) = coStr(2)(1:1)
	coStr4(2:2) = coStr(2)(1:1)[coStr_index(1),coStr_index(2),coStr_index(3)]
	coStr4(3:3)[coStr4_index(1),coStr4_index(2)] = coStr(2)(1:1)
	coStr4(4:4)[coStr4_index(1),coStr4_index(2)] = coStr(2)(1:1)[coStr_index(1),coStr_index(2),coStr_index(3)]

	if(.NOT.( (len_trim(coStr4) == 4 ) .AND. (verifyChars(coStr4,1,4,parity_char)))) then
		error stop 31
	end if

	coStr4 = ""
	coStr4(1:1) = coStr(2)(1:1)[-1,0,2]
	coStr4(2:2)[coStr4_index(1),coStr4_index(2)] = coStr(2)(1:1)[-1,0,2]

	if(.NOT.( (len_trim(coStr4) == 2 ) .AND. (verifyChars(coStr4,1,2,P_parity_char)))) then
		error stop 32
	end if

	!!! ************  last character  ************ !!!

	coStr4 = ""
	coStr4(1:1) = coStr(2)(100:100)
	coStr4(2:2) = coStr(2)(100:100)[coStr_index(1),coStr_index(2),coStr_index(3)]
	coStr4(3:3)[coStr4_index(1),coStr4_index(2)] = coStr(2)(100:100)
	coStr4(4:4)[coStr4_index(1),coStr4_index(2)] = coStr(2)(100:100)[coStr_index(1),coStr_index(2),coStr_index(3)]

	if(.NOT.( (len_trim(coStr4) == 4 ) .AND. (verifyChars(coStr4,1,4,parity_char)))) then
		error stop 33
	end if

	coStr4 = ""
	coStr4(1:1) = coStr(2)(100:100)[-1,0,2]
	coStr4(2:2)[coStr4_index(1),coStr4_index(2)] = coStr(2)(100:100)[-1,0,2]

	if(.NOT.( (len_trim(coStr4) == 2 ) .AND. (verifyChars(coStr4,1,2,P_parity_char)))) then
		error stop 34
	end if

	!!! ************  middle single character  ************ !!!

	coStr4 = ""
	coStr4(1:1) = coStr(2)(50:50)
	coStr4(2:2) = coStr(2)(50:50)[coStr_index(1),coStr_index(2),coStr_index(3)]
	coStr4(3:3)[coStr4_index(1),coStr4_index(2)] = coStr(2)(50:50)
	coStr4(4:4)[coStr4_index(1),coStr4_index(2)] = coStr(2)(50:50)[coStr_index(1),coStr_index(2),coStr_index(3)]

	if(.NOT.( (len_trim(coStr4) == 4 ) .AND. (verifyChars(coStr4,1,4,parity_char)))) then
		error stop 35
	end if

	coStr4 = ""
	coStr4(1:1) = coStr(2)(50:50)[-1,0,2]
	coStr4(2:2)[coStr4_index(1),coStr4_index(2)] = coStr(2)(50:50)[-1,0,2]

	if(.NOT.( (len_trim(coStr4) == 2 ) .AND. (verifyChars(coStr4,1,2,P_parity_char)))) then
		error stop 36
	end if

	!!! ************  middle many characters   ************ !!!

	coStr4 = ""
	coStr4(1:5) = coStr(2)(48:52)
	coStr4(6:10) = coStr(2)(48:52)[coStr_index(1),coStr_index(2),coStr_index(3)]
	coStr4(11:15)[coStr4_index(1),coStr4_index(2)] = coStr(2)(48:52)
	coStr4(16:20)[coStr4_index(1),coStr4_index(2)] = coStr(2)(48:52)[coStr_index(1),coStr_index(2),coStr_index(3)]

	if(.NOT.( (len_trim(coStr4) == 20 ) .AND. (verifyChars(coStr4,1,20,parity_char)))) then
		error stop 37
	end if

	coStr4 = ""
	coStr4(1:5) = coStr(2)(48:52)[-1,0,2]
	coStr4(6:10)[coStr4_index(1),coStr4_index(2)] = coStr(2)(48:52)[-1,0,2]

	if(.NOT.( (len_trim(coStr4) == 10 ) .AND. (verifyChars(coStr4,1,10,P_parity_char)))) then
		error stop 38
	end if

	!!! ************  some string including the first character   ************ !!!

	coStr4 = ""
	coStr4(1:5) = coStr(2)(1:5)
	coStr4(6:10) = coStr(2)(1:5)[coStr_index(1),coStr_index(2),coStr_index(3)]
	coStr4(11:15)[coStr4_index(1),coStr4_index(2)] = coStr(2)(1:5)
	coStr4(16:20)[coStr4_index(1),coStr4_index(2)] = coStr(2)(1:5)[coStr_index(1),coStr_index(2),coStr_index(3)]

	if(.NOT.( (len_trim(coStr4) == 20 ) .AND. (verifyChars(coStr4,1,20,parity_char)))) then
		error stop 39
	end if

	coStr4 = ""
	coStr4(1:5) = coStr(2)(1:5)[-1,0,2]
	coStr4(6:10)[coStr4_index(1),coStr4_index(2)] = coStr(2)(1:5)[-1,0,2]

	if(.NOT.( (len_trim(coStr4) == 10 ) .AND. (verifyChars(coStr4,1,10,P_parity_char)))) then
		error stop 40
	end if

	!!! ************  some string including the last character   ************ !!!

	coStr4 = ""
	coStr4(1:5) = coStr(2)(96:100)
	coStr4(6:10) = coStr(2)(96:100)[coStr_index(1),coStr_index(2),coStr_index(3)]
	coStr4(11:15)[coStr4_index(1),coStr4_index(2)] = coStr(2)(96:100)
	coStr4(16:20)[coStr4_index(1),coStr4_index(2)] = coStr(2)(96:100)[coStr_index(1),coStr_index(2),coStr_index(3)]

	if(.NOT.( (len_trim(coStr4) == 20 ) .AND. (verifyChars(coStr4,1,20,parity_char)))) then
		error stop 41
	end if

	coStr4 = ""
	coStr4(1:5) = coStr(2)(96:100)[-1,0,2]
	coStr4(6:10)[coStr4_index(1),coStr4_index(2)] = coStr(2)(96:100)[-1,0,2]

	if(.NOT.( (len_trim(coStr4) == 10 ) .AND. (verifyChars(coStr4,1,10,P_parity_char)))) then
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
		loop1:do k = coStr_low_bounds(3),coStr_up_bounds(3)
				do j = coStr_low_bounds(2),coStr_up_bounds(2)
					do i = coStr_low_bounds(1),coStr_up_bounds(1)
						coStr4 = trim(coStr4) // coStr(1)(1:5)[i,j,k]
						m = image_index(coStr,(/i,j,k/))
						if(m == ne) then
							exit loop1
						end if
					end do
				end do
			end do loop1
		
		if(len_trim(coStr4) /= (ne * 5)) then
			error stop 51        
		end if
		
		loStr = ""
		loop2:do k = coStr_low_bounds(3),coStr_up_bounds(3)
				do j = coStr_low_bounds(2),coStr_up_bounds(2)
					do i = coStr_low_bounds(1),coStr_up_bounds(1)
						m = image_index(coStr,(/i,j,k/))
						loStr((m-1)*5+1:i*5) = coStr(1)(6:10)[i,j,k]
						if(m == ne) then
							exit loop2
						end if
					end do
				end do
			end do loop2
		
		if(coStr4 /= loStr) then 
		   error stop 52
		end if
		
	else if (me /= P) then
		! when current image is Q, substring from P & Q, then change P	
		if(me == Q) then 
			coStr4[0,1] = ""
			coStr4[0,1] = coStr(1)(1:5)[-1,0,2] // coStr(1)(6:10)[0,0,2]
			if(.NOT.( (len_trim(coStr4[0,1]) == 10) .AND. (verifyChars(coStr4[0,1],1,5,P_unit_char)) &
			  .AND. (verifyChars(coStr4[0,1],6,10,Q_unit_char)) )) then
				error stop 53
			end if
			
			coStr4[0,1] = ""
			coStr4[0,1] = coStr(1)(3:7)[-1,0,2] // coStr(1)(3:7)[0,0,2]
			if(.NOT.( (len_trim(coStr4[0,1]) == 10) .AND. (verifyChars(coStr4[0,1],1,5,P_unit_char)) &
			  .AND. (verifyChars(coStr4[0,1],6,10,Q_unit_char)) )) then
				error stop 54
			end if
			
			coStr4[0,1] = ""
			coStr4[0,1] = coStr(1)(6:10)[-1,0,2] // coStr(1)(1:5)[0,0,2]
			if(.NOT.( (len_trim(coStr4[0,1]) == 10) .AND. (verifyChars(coStr4[0,1],1,5,P_unit_char)) &
			  .AND. (verifyChars(coStr4[0,1],6,10,Q_unit_char)) )) then
				error stop 55
			end if
		end if
		
	end if

end program substr003
