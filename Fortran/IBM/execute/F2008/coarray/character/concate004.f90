!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : concate004.f
!*
!*  DATE                       : March 23, 2011
!*  ORIGIN                     : Compiler Development, IBM CDL
!*
!*  PRIMARY FUNCTIONS TESTED   : Test the concatenation of character array coarray
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : No Feature Number
!*
!*  REQUIRED COMPILER OPTIONS  : -qcaf -q64
!*
!*  KEYWORD(S)                 : character, CAF, array
!*
!*  TARGET(S)                  : character array coarray
!*
!*  DESCRIPTION:
!*  -----------
!*  The testcase aim to
!*  1. test the concatenation of character array coarray
!*  -----------
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program concate004
	use char_mod
	implicit none

	integer, parameter  :: P = 1, Q = 2

	character (len=10), save :: coStr(3)[-1:0,0:1,2:*]
	character (len=10) 		 :: loStr(2)

	character (len=1000) 		:: concateStr
	character (len=1000) 		:: verifyConcateStr
	character (len=1000), save 	:: coConcateStr[0:2,1:*]

	character (len=1),	  save 	:: co_verify_chars(-1:0)[*]
	character (len=1) 			:: PQ_verify_chars(0:3)
	character (len=1) 			:: unit_char, parity_char

	integer :: coStr_low_bounds(3), coStr_up_bounds(3), coStr_index(3)
	integer :: coConcateStr_low_bounds(2), coConcateStr_up_bounds(2), coConcateStr_index(2)

	integer :: me, ne, units, i, j, k, m, n

	me = this_image()
	ne = num_images()

	units = MOD(me,10)
	co_verify_chars(-1) = ACHAR(units+48)
	unit_char = ACHAR(units+48)

	if ( MOD(me,2) == 0) then
		co_verify_chars(0) = "E"
		parity_char = "E"
	else
		co_verify_chars(0) = "O"
		parity_char = "O"
	end if

	PQ_verify_chars(0) = '1'
	PQ_verify_chars(1) = 'O'
	PQ_verify_chars(2) = '2'
	PQ_verify_chars(3) = 'E'

	!coStr(1) = repeat(co_verify_chars(-1),10)
	!coStr(2) = repeat(co_verify_chars(0),10)
	!coStr(3) = repeat(co_verify_chars(-1),10)
	!loStr(1) = repeat(co_verify_chars(0),10)
	!loStr(2) = repeat(co_verify_chars(-1),10)

	coStr(1) = repeat(unit_char,10)
	coStr(2) = repeat(parity_char,10)
	coStr(3) = repeat(unit_char,10)
	loStr(1) = repeat(parity_char,10)
	loStr(2) = repeat(unit_char,10)

	verifyConcateStr = ""
	coConcateStr = ""

	coStr_low_bounds = lcobound(coStr)
	coStr_up_bounds = ucobound(coStr)

	coConcateStr_low_bounds = lcobound(coConcateStr)
	coConcateStr_up_bounds = ucobound(coConcateStr)

	coStr_index = this_image(coStr)
	coConcateStr_index = this_image(coConcateStr)

	sync all

	!!! ************  coarray // local  ************ !!!
	concateStr = ""
	concateStr = coStr(1)//loStr(1) ! 10 + 10
	if(.NOT.( (len_trim(concateStr) == 20) .AND. (verifyChars(concateStr,1,10,co_verify_chars(-1))) &
	  .AND. (verifyChars(concateStr,11,20,co_verify_chars(0))))) then
		error stop 11
	end if

	!!! ************  coarray[me] // local  ************ !!!
	concateStr = ""
	concateStr = coStr(1)[coStr_index(1),coStr_index(2),coStr_index(3)]//loStr(1) ! 10 +
	if(.NOT.( (len_trim(concateStr) == 20) .AND. (verifyChars(concateStr,1,10,co_verify_chars(-1))) &
	  .AND. (verifyChars(concateStr,11,20,co_verify_chars(0))))) then
		error stop 12
	end if

	!!! ************  coarray[P] // local  ************ !!!
	concateStr = ""
	concateStr = coStr(1)[-1,0,2]//loStr(1) ! 10 + 10
	if(.NOT.( (len_trim(concateStr) == 20) .AND. (verifyChars(concateStr,1,10,PQ_verify_chars(0))) &
	  .AND. (verifyChars(concateStr,11,20,co_verify_chars(0))))) then
		! print *, "index=",me,"\t ",concateStr(1:10),(len_trim(concateStr) == 20), (verifyChars(concateStr,1,10,PQ_verify_chars(0))), (verifyChars(concateStr,11,20,co_verify_chars(0)))
		 error stop 13
	end if

	!!! ************  coarray // coarray[P]  ************ !!!
	concateStr = ""
	concateStr = coStr(1)//coStr(2)[-1,0,2] ! 10 + 10
	if(.NOT.( (len_trim(concateStr) == 20) .AND. (verifyChars(concateStr,1,10,co_verify_chars(-1))) &
	  .AND. (verifyChars(concateStr,11,20,PQ_verify_chars(1))))) then
		error stop 14
	end if

	!!! ************  coarray[me] // coarray[P]  ************ !!!
	concateStr = ""
	concateStr = coStr(1)[coStr_index(1),coStr_index(2),coStr_index(3)]//coStr(2)[-1,0,2]
	! 10 + 10
	if(.NOT.( (len_trim(concateStr) == 20) .AND. (verifyChars(concateStr,1,10,co_verify_chars(-1))) &
	  .AND. (verifyChars(concateStr,11,20,PQ_verify_chars(1))))) then
		error stop 15
	end if

	!!! ************  coarray // coarray   ************ !!!
	concateStr = ""
	concateStr = coStr(1)//coStr(2) ! 10 + 10
	if(.NOT.( (len_trim(concateStr) == 20) .AND. (verifyChars(concateStr,1,10,co_verify_chars(-1))) &
	  .AND. (verifyChars(concateStr,11,20,co_verify_chars(0))))) then
		error stop 16
	end if

	concateStr = ""
	concateStr = coStr(1)//coStr(2)//coStr(3) ! 10 + 10 + 10
	if(.NOT.( (len_trim(concateStr) == 30) .AND. (verifyChars(concateStr,1,10,co_verify_chars(-1))) &
	  .AND. (verifyChars(concateStr,11,20,co_verify_chars(0))) &
	  .AND. (verifyChars(concateStr,21,30,co_verify_chars(-1))))) then
		error stop 17
	end if

	!!! ************  coarray // coarray[me]   ************ !!!
	concateStr = ""
	concateStr = coStr(1)//coStr(2)[coStr_index(1),coStr_index(2),coStr_index(3)] ! 10+10
	if(.NOT.( (len_trim(concateStr) == 20) .AND. (verifyChars(concateStr,1,10,co_verify_chars(-1))) &
	  .AND. (verifyChars(concateStr,11,20,co_verify_chars(0))))) then
		error stop 18
	end if

	!!! ************  coarray[me] // coarray[me]  ************ !!!
	concateStr = ""
	concateStr = coStr(1)[coStr_index(1),coStr_index(2),coStr_index(3)]//coStr(2)[coStr_index(1),coStr_index(2),coStr_index(3)]
	! 10+10
	if(.NOT.( (len_trim(concateStr) == 20) .AND. (verifyChars(concateStr,1,10,co_verify_chars(-1))) &
	  .AND. (verifyChars(concateStr,11,20,co_verify_chars(0))))) then
		error stop 19
	end if

	!!! ************  SELF concatenation  ************ !!!

	concateStr = ""
	concateStr = coStr(1)//coStr(1) ! 10 + 10
	if(.NOT.( (len_trim(concateStr) == 20) .AND. (verifyChars(concateStr,1,20,co_verify_chars(-1))))) then
		error stop 31
	end if

	concateStr = ""
	concateStr = coStr(1)//coStr(1)[coStr_index(1),coStr_index(2),coStr_index(3)]
	if(.NOT.( (len_trim(concateStr) == 20) .AND. (verifyChars(concateStr,1,20,co_verify_chars(-1))))) then
		error stop 32
	end if

	concateStr = ""
	concateStr = coStr(1)[coStr_index(1),coStr_index(2),coStr_index(3)]//coStr(1)[coStr_index(1),coStr_index(2),coStr_index(3)]
	if(.NOT.( (len_trim(concateStr) == 20) .AND. (verifyChars(concateStr,1,20,co_verify_chars(-1))))) then
		error stop 33
	end if

	! ************** blend images ************** !

	if (ne > MAX_IMAGE) then
		ne = MAX_IMAGE
	end if

	sync all

	! when current image is P, concatenate all the other images
	if(me == P) then

		concateStr = ""

		loop1:do k = coStr_low_bounds(3),coStr_up_bounds(3)
				do j = coStr_low_bounds(2),coStr_up_bounds(2)
					do i = coStr_low_bounds(1),coStr_up_bounds(1)
						concateStr = trim(concateStr) // coStr(1)[i,j,k]
						m = image_index(coStr,(/i,j,k/))
						if(m == ne) then
							exit loop1
						end if
					end do
				end do
			end do loop1

		if(len_trim(concateStr) /= (ne * 10)) then
			error stop 41
		end if

		loop2:do k = coStr_low_bounds(3),coStr_up_bounds(3)
				do j = coStr_low_bounds(2),coStr_up_bounds(2)
					do i = coStr_low_bounds(1),coStr_up_bounds(1)
						m = image_index(coStr,(/i,j,k/))
						verifyConcateStr((m-1)*10+1:m*10) = coStr(1)[i,j,k]
						if(m == ne) then
							exit loop2
						end if
					end do
				end do
			end do loop2

		if(concateStr /= verifyConcateStr) then
		   error stop 42
		end if

	! when current is not P, change P
	else if (me /= P) then

		if(me == Q) then
			coConcateStr[0,1] = coStr(1)[-1,0,2]//coStr(1)[0,0,2]
			if(.NOT.( (len_trim(coConcateStr[0,1]) == 20) .AND. (verifyChars(coConcateStr[0,1],1,10,PQ_verify_chars(0))) &
			  .AND. (verifyChars(coConcateStr[0,1],11,20,PQ_verify_chars(2))) )) then
				error stop 43
			end if

			coConcateStr[0,1]  = trim(coConcateStr[0,1])//coStr(2)[0,0,2]//coStr(2)[-1,0,2]

			if(.NOT.( (len_trim(coConcateStr[0,1]) == 40) .AND. (verifyChars(coConcateStr[0,1],1,10,PQ_verify_chars(0))) &
			  .AND. (verifyChars(coConcateStr[0,1],11,20,PQ_verify_chars(2)))  &
			  .AND. (verifyChars(coConcateStr[0,1],21,30,PQ_verify_chars(3))) &
			  .AND. (verifyChars(coConcateStr[0,1],31,40,PQ_verify_chars(1))) )) then
				error stop 44
			end if
		end if

	end if

	sync all

	! concatenation between any two images
	loop3:  do k = coStr_low_bounds(3),coStr_up_bounds(3)
				do j = coStr_low_bounds(2),coStr_up_bounds(2)
					do i = coStr_low_bounds(1),coStr_up_bounds(1)

						m = image_index(coStr,(/i,j,k/))

						coConcateStr = ""
						coConcateStr = coStr(2)//coStr(2)[i,j,k]

						if(len_trim(coConcateStr) /= 20) then
							error stop 45
						end if

						if(.NOT. (verifyChars(coConcateStr,1,10,co_verify_chars(0)))) then
							error stop 46
						end if

						if (MOD(m,2) == 0) then
							if(.NOT. (verifyChars(coConcateStr,11,20,PQ_verify_chars(3)))) then
								error stop 47
							end if
						else
							if(.NOT. (verifyChars(coConcateStr,11,20,PQ_verify_chars(1)))) then
								error stop 48
							end if
						end if

						if(m == ne) then
							exit loop3
						end if
					end do
				end do
			end do loop3

end program concate004
