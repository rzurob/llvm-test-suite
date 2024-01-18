!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArg002.f
!*  TEST CASE TITLE            : Test the dummy arg character array coarray
!*                               
!*  PROGRAMMER                 : Ke Wen Lin 
!*  DATE                       : March 28, 2011 
!*  ORIGIN                     : Compiler Development, IBM CDL
!*
!*  PRIMARY FUNCTIONS TESTED   : Test the dummy arg character array coarray 
!*                              
!*  SECONDARY FUNCTIONS TESTED :                                                                              
!*  
!*  REFERENCE                  : No Feature Number
!*
!*  DRIVER STANZA              : xlf2003_r
!*  REQUIRED COMPILER OPTIONS  : -qcaf -q64
!*
!*  KEYWORD(S)                 : character, CAF, substring
!*                                        
!*  TARGET(S)                  : different length character                           
!*
!*  DESCRIPTION:
!*  -----------
!*  The testcase aim to 
!*  1. test the dummy arg character array coarray
!*  -----------
!*  
!234567890123456789012345678901234567890123456789012345678901234567890

program dummyArg002
	implicit none
	
	integer, parameter :: P = 1, Q = 2, ArrSize = 3
	
	character (len=10), dimension(ArrSize), 				save :: coStr1[*]
	character (len=10), dimension(ArrSize,ArrSize), 		save :: coStr2[0:2,1:*]
	character (len=10), dimension(ArrSize,ArrSize,ArrSize), save :: coStr3[-1:0,0:1,2:*] 
	
	character (len=1),   save :: unit_char[*], parity_char[*]  
	character (len=20),  save :: coConcateStr[*]
	
	character (len=1)   :: P_unit_char 		= '1'
	character (len=1)   :: P_parity_char 	= 'O'
	character (len=1)   :: Q_unit_char 		= '2'
	character (len=1)   :: Q_parity_char 	= 'E'
	character (len=10)  :: verifyStr1, verifyStr2
	
	integer :: me, ne, units, i, m, n, l
    logical :: funcRet

	me = this_image()
	ne = num_images()

	units = MOD(me,10)
	unit_char = ACHAR(units+48)

	if (MOD(me,2) == 0) then
		parity_char = "E"
	else 
		parity_char = "O"
	end if
	
	sync all
	
	if (ne < max(P,Q)) error stop 2
		
	if(me == P) then
		call sub(coStr1, coStr2, coStr3, unit_char, parity_char, P)
		verifyStr1 = repeat(P_unit_char,10)
		verifyStr2 = repeat(P_parity_char,10)
		funcRet = func(coStr1, coStr2, coStr3, verifyStr1, verifyStr2)
	end if
	
	sync all 
	
	if(me == Q) then
		call sub(coStr1, coStr2, coStr3, unit_char, parity_char, Q)
	end if

	sync all
	
	verifyStr1 = repeat(Q_unit_char,10)
	verifyStr2 = repeat(Q_parity_char,10)
	
	do m = 1, ArrSize
		if(coStr1(m) /= verifyStr1) then
			error stop 21
		end if
	end do
	
	do m = 1, ArrSize
		do n = 1, ArrSize
			if(coStr2(m,n) /= verifyStr2) then
				error stop 22
			end if
		end do 
	end do
	
	do m = 1, ArrSize
		do n = 1, ArrSize
			do l = 1, ArrSize
				if(coStr3(m,n,l) /= verifyStr1) then
					error stop 23
				end if
			end do
		end do 
	end do
	
  contains 
	
	! set coarray co_str1, co_str2, co_str3 using coarray char1, char2 on image im
	subroutine sub(co_str1, co_str2, co_str3, char1, char2, im)
		character(len=10), dimension(ArrSize), intent(inout) :: co_str1[*]
		character(len=10), dimension(ArrSize,*), intent(inout) :: co_str2[0:2,1:*]
		character(len=10), dimension(:,:,:), intent(inout) :: co_str3[-1:0,0:1,2:*]
		character(len=1), intent(in) :: char1[*], char2[*]
		
		integer, intent(in) :: im
	
		integer :: i, j, k, m, n, l
		integer :: lo1(1), up1(1), lo2(2), up2(2), lo3(3), up3(3)
		
		lo1 = lcobound(co_str1); lo2 = lcobound(co_str2); lo3 = lcobound(co_str3)
		up1 = ucobound(co_str1); up2 = ucobound(co_str2); up3 = ucobound(co_str3)
		
		if(any (lo1 .NE. [1]) .OR. any(lo2 .NE. [0,1]) .OR. any(lo3 .NE. [-1,0,2])) then
			error stop 11
		end if
		
		do i = lo1(1), up1(1)
			do m = 1, ArrSize
				co_str1(m)[i] = repeat(char1[im],10)
			end do
		end do
		
		do j = lo2(2), up2(2)
			do i = lo2(1), up2(1)
				do m = 1, ArrSize
					do n = 1, ArrSize
						co_str2(m,n)[i,j] = repeat(char2[im],10)
					end do 
				end do
			end do
		end do
		
		do k = lo3(3), up3(3)
			do j = lo3(2), up3(2)
				do i = lo3(1), up3(1)
					do m = 1, ArrSize
						do n = 1, ArrSize
							do l = 1, ArrSize
								co_str3(m,n,l)[i,j,k] = repeat(char1[im],10)
							end do
						end do 
					end do
				end do
			end do
		end do
		
	end subroutine sub
  
	! verify coarray co_str1, co_str2, co_str3 using str1, str2
	function func(co_str1, co_str2, co_str3, str1, str2)
		character(len=10), dimension(:), intent(in) :: co_str1[*]
		character(len=10), dimension(ArrSize,ArrSize), intent(in) :: co_str2[0:2,1:*]
		character(len=10), dimension(ArrSize,ArrSize,*), intent(in) :: co_str3[-1:0,0:1,2:*]
		character(len=10), intent(in) :: str1, str2
		
		logical :: func
		
		integer :: i, j, k, m, n, l
		integer :: lo1, up1, lo2(2), up2(2), lo3(3), up3(3)
		
		lo1 = lcobound(co_str1,1); lo2 = lcobound(co_str2); lo3 = lcobound(co_str3);
		up1 = ucobound(co_str1,1); up2 = ucobound(co_str2); up3 = ucobound(co_str3);
		
		if(lo1 /= 1 .OR. any(lo2 .NE. [0,1]) .OR. any(lo3 .NE. [-1,0,2])) then
			error stop 12
		end if
		
		do i = lo1, up1
			do m = 1, ArrSize
				if(co_str1(m)[i] /= str1) then
					error stop 13
				end if
			end do
		end do
		
		do j = lo2(2), up2(2)
			do i = lo2(1), up2(1)
				do m = 1, ArrSize
					do n = 1, ArrSize
						if(co_str2(m,n)[i,j] /= str2) then
							error stop 14
						end if
					end do 
				end do
			end do
		end do
		
		do k = lo3(3), up3(3)
			do j = lo3(2), up3(2)
				do i = lo3(1), up3(1)
					do m = 1, ArrSize
						do n = 1, ArrSize
							do l = 1, ArrSize
								if(co_str3(m,n,l)[i,j,k] /= str1) then
									error stop 15
								end if
							end do
						end do 
					end do
				end do
			end do
		end do
		
        func = .TRUE.
	end function func
		
end program dummyArg002
