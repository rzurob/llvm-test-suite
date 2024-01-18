!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : module_for_char.f
!*  TEST CASE TITLE            : Auxiliary module for character coarray testing 
!*                               
!*  PROGRAMMER                 : Ke Wen Lin 
!*  DATE                       : March 16, 2011 
!*  ORIGIN                     : Compiler Development, IBM CDL
!*
!*  PRIMARY FUNCTIONS TESTED   : Auxiliary module for character coarray testing
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
!*  TARGET(S)                  :     
!*
!*  DESCRIPTION:
!*  -----------
!*  The testcase aim to 
!*  1. provide auxiliary module for character coarray testing
!*  -----------
!*  
!234567890123456789012345678901234567890123456789012345678901234567890

module char_mod
	implicit none
	integer, parameter :: MAX_IMAGE = 16
  contains

	! This method is to convert an integer number to a character
	! such as: intValue = 123, retChar = '123'
	subroutine getCharArray (intValue, retChar)
	
		implicit none
		integer, intent(in)::intValue
		character(len=3),intent(inout)::retChar
		integer,dimension(3) :: digits
		integer :: i
		logical :: hitZero

		i = 1
		hitZero = .true.

		if (intValue < 0) then
			error stop "subroutine getCharArray require positive integer arg"
		endif

		if (intValue > MAX_IMAGE) then
			error stop "subroutine getCharArray limit integer arg must less than 999"
		endif

		digits(3) = MOD (intValue,10)
		digits(2) = MOD (intValue,100)/10
		digits(1) = intValue/100

		!print *, digits,hitZero

		do i = 1, 3
		   if(hitZero .and. digits(i) /= 0) then
			  hitZero = .false.
		   endif

		   if(hitZero) then
			  retChar(i:i) = ' '
		   else
			  retChar(i:i) = ACHAR(digits(i)+48)
		   endif

		   if((i == 3) .and. (len_trim(retChar) == 0)) then
			  retChar(3:3) = '0'
		   endif
		end do

	end subroutine getCharArray

	logical function verifyChars(chars, start_pos, end_pos, single_char)
		character(len=*), intent(in) :: chars
		integer, intent(in) :: start_pos, end_pos
		character(len=1), intent(in) :: single_char

		integer :: i = 0
		verifyChars = .true.
		do i = start_pos, end_pos, 1
		   if(chars(i:i) /= single_char) then 
				verifyChars = .false.
		   end if     
		end do
	end function verifyChars

	subroutine initializeChar(chars, start_pos, end_pos, single_char)
		character(len=*), intent(inout) :: chars
		integer, intent(in) :: start_pos, end_pos
		character(len=1), intent(in) :: single_char
		integer :: i     

		do i = start_pos, end_pos, 1
			chars(i:i) = single_char
		end do
	end subroutine initializeChar

end module

