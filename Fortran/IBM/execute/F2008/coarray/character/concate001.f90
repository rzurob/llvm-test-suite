!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : concate001.f
!*  TEST CASE TITLE            : Test the concatenation of zero-length character coarray
!*                               
!*  PROGRAMMER                 : Ke Wen Lin 
!*  DATE                       : March 16, 2011 
!*  ORIGIN                     : Compiler Development, IBM CDL
!*
!*  PRIMARY FUNCTIONS TESTED   : Test the concatenation of zero-length character coarray
!*                              
!*  SECONDARY FUNCTIONS TESTED :                                                      
!*                              
!*  REFERENCE                  : No Feature Number
!*
!*  DRIVER STANZA              : xlf2003_r
!*  REQUIRED COMPILER OPTIONS  : -qcaf -q64
!*
!*  KEYWORD(S)                 : character, CAF
!*                                                          
!*  TARGET(S)                  : zero-length character                           
!*
!*  DESCRIPTION:
!*  -----------
!*  The testcase aim to 
!*  1. test the concatenation of zero-length character coarray.
!*  2. verify that result when zero-length character coarray concatenate with other local character or characters on other images.
!*  -----------
!*  
!234567890123456789012345678901234567890123456789012345678901234567890

program concate001
	use char_mod
	implicit none

	integer, parameter :: P = 1, Q = 2
	
	character (len=0), save :: coZeroStr1[*]
	character (len=0), save :: coZeroStr2[*]
	character (len=0) 		:: loZeroStr1
	character (len=0) 		:: loZeroStr2
	
	character (len=100) 		:: concateStr
	character (len=100) 		:: verifyConcateStr
	character (len=100), save 	:: coConcateStr[*]

	character (len=3), save :: imageIndexStr[*]
	character (len=3) 		:: localImageIndexStr

	integer :: me, ne, i

	me = this_image()
	ne = num_images()

	concateStr = ""
	verifyConcateStr = ""
	coConcateStr = ""

	coZeroStr1 = ""
	coZeroStr2 = "non zero length character" ! should be zero although assign non-length character
	loZeroStr1 = ""
	loZeroStr2 = "non zero length character" ! should be zero although assign non-length character

	sync all
	
	if (.NOT. ((len(coZeroStr1) == 0) .AND. (len(loZeroStr1) == 0))) then
		 error stop "non zero-length characters"
	end if 

	if (.NOT. ((coZeroStr1 == coZeroStr2) .AND. (loZeroStr1 == loZeroStr2))) then 
		 error stop "non zero-length characters"
	end if 

	! misc concatenation
	if (len(coZeroStr1//loZeroStr1) /= 0) then
		error stop 10
	end if

	if (len(coZeroStr1[me]//loZeroStr1) /= 0) then
		error stop 11
	end if

	if (len(coZeroStr1[P]//loZeroStr1) /= 0) then
		error stop 12
	end if

	if (len(coZeroStr1//coZeroStr1[P]) /= 0) then
		error stop 13
	end if

	if (len(coZeroStr1[me]//coZeroStr1[P]) /= 0) then
		error stop 14
	end if

	if (len(coZeroStr1//coZeroStr2) /= 0) then
		error stop 15
	end if

	if (len(coZeroStr1[me]//coZeroStr2[me]) /= 0) then
		error stop 16
	end if

	if (len(coZeroStr1//coZeroStr2[me]) /= 0) then
		error stop 17
	end if

	! string self concatenation
	if (len(coZeroStr1//coZeroStr1) /= 0) then
		error stop 18
	end if

	if (len(coZeroStr1[me]//coZeroStr1[me]) /= 0) then
		error stop 19
	end if

	if (len(coZeroStr1//coZeroStr1[me]) /= 0) then
		error stop 20
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

		do i = 1, ne
			concateStr = trim(concateStr) // coZeroStr1[i]
		end do 
			
		if(len_trim(concateStr) /= 0) then
			error stop 31        
		end if

		do i = 1, ne
		   verifyConcateStr = trim(verifyConcateStr) // trim(imageIndexStr[i])
		end do

		do i = 1, ne
		   concateStr = loZeroStr1 // trim(concateStr) // coZeroStr1[i] // trim(imageIndexStr[i]) // coZeroStr1[me]
		end do
		
		if(concateStr /= verifyConcateStr) then 
		   error stop 32
		end if
	
	! when current is not P, change P 	
	else if (me /= P) then 
		
		coConcateStr[P] = trim(coConcateStr[P])//coZeroStr1//loZeroStr1//coConcateStr[Q]
		
		if(me == Q) then 
			if(len_trim(coConcateStr[P]) /= 0) then
				error stop 33
			end if
		end if
		
	end if

	sync all 
	
	! concatenate backward 
	do i = me, ne
		verifyConcateStr = trim(verifyConcateStr) // trim(imageIndexStr[i])    
	end do 

	do i = me, ne
		concateStr = trim(concateStr) //  coZeroStr1[i] // trim(imageIndexStr[i]) // coZeroStr1[i]
	end do

	if(verifyConcateStr /= concateStr) then
		error stop 34
	end if

	sync all
	
	! concatenation between any two images
	do i = 1, ne
		if(len_trim(coZeroStr1[i]//coZeroStr1) /= 0) then 
			error stop 35
		end if
		
		if(len_trim(coZeroStr1[i]//coZeroStr1[P]) /= 0) then 
			error stop 36
		end if
	end do 

end program concate001
