!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 28, 2011
!*
!*  PRIMARY FUNCTIONS TESTED   : Test the dummy arg character coarray
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
!*  1. test the dummy arg character coarray
!*  -----------
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module dummy_mod
	implicit none
	character (len=100), save :: coStr2[*]

  contains

	subroutine sub4(co_str1, co_str2, concate_str, im)
		integer :: im
		character(10), intent(in) :: co_str1[*], co_str2[*]
		character(20), intent(inout) :: concate_str
		concate_str = co_str1[im] // co_str2[im]
	end subroutine sub4

	function func4(co_str1, co_str2, concate_str, im)
		integer :: im
		character(10), intent(in) :: co_str1[*], co_str2[*]
		character(20), intent(in) :: concate_str
		character(20) :: tmp_str
        logical :: func4

		tmp_str = co_str1[im] // co_str2[im]

		if(tmp_str /= concate_str) then
			func4 = .FALSE.
		else
			func4 = .TRUE.
		end if
	end function func4

end module dummy_mod

program dummyArg001
	use char_mod
	use dummy_mod
	implicit none

	integer, parameter :: P = 1, Q = 2

	character (len=10),  save :: coStr1[*]
	character (len=1),   save :: unit_char[*], parity_char[*]
	character (len=110), save :: coConcateStr[*]

	character (len=1)   :: P_unit_char 		= '1'
	character (len=1)   :: P_parity_char 	= 'O'
	character (len=1)   :: Q_unit_char		= '2'
	character (len=1)   :: Q_parity_char 	= 'E'
	character (len=1)   :: subStr
	integer 		    :: strLen
	character (len=20)  :: concateStr1
	character (len=200) :: concateStr2

	integer :: me, ne, i, units

    interface
        subroutine sub3(co_str1, co_str2, concate_str, im)
            integer :: im
            character(100), intent(in) :: co_str1[*], co_str2[*]
            character(200), intent(inout) :: concate_str
        end subroutine sub3

        function func3(co_str1, co_str2, concate_str, im)
            integer :: im
            character(100), intent(in) :: co_str1[*], co_str2[*]
            character(200), intent(in) :: concate_str
            character(200) :: tmp_str
            logical :: func3
        end function func3
    end interface

	me = this_image()
	ne = num_images()

	units = MOD(me,10)
	unit_char = ACHAR(units+48)

	if (MOD(me,2) == 0) then
		parity_char = "E"
	else
		parity_char = "O"
	end if

	coStr1 = repeat(unit_char,len(coStr1))
	coStr2 = repeat(parity_char,len(coStr2))

	sync all

	if (ne < max(P,Q)) error stop 2

	call sub1(coStr1, strLen, subStr, Q)

	if(strLen /= 10 .OR. subStr /= Q_unit_char) then
		error stop 11
	end if

	if(.NOT. func1(coStr1, strLen, subStr, Q)) then
		error stop 12
	end if

	if(me == Q) then
		call sub2(coStr1, coStr2, coConcateStr, P, Q)
		if(len_trim(coConcateStr[P]) /= 110 .OR. (.NOT. verifyChars(coConcateStr[P],1,10,Q_unit_char)) &
			.OR. (.NOT. verifyChars(coConcateStr[P],11,110,Q_parity_char))) then
			error stop 13
		end if
	end if

	sync all

	if(.NOT. func2(coStr1, coStr2, coConcateStr, P, Q)) then
		error stop 14
	end if

	! ******* module subroutines & functions ******* !

	call sub3(coStr2, coStr2, concateStr2, P)

	if(len_trim(concateStr2) /= 200 .OR. (.NOT. verifyChars(concateStr2,1,200,P_parity_char))) then
		error stop 15
	end if

	if(.NOT. func3(coStr2, coStr2, concateStr2, P)) then
		error stop 16
	end if

	! ******* external subroutines & functions ******* !

	call sub4(coStr1, coStr1, concateStr1, Q)

	if(len_trim(concateStr2) /= 20 .OR. (.NOT. verifyChars(concateStr1,1,20,Q_unit_char))) then
		error stop 17
	end if

	if(.NOT. func4(coStr2, coStr2, concateStr2, P)) then
		error stop 18
	end if

  contains

	subroutine sub1(co_str, str_len, str_char, im)
		integer, intent(in) :: im
		character(10), intent(in) :: co_str[*]
		integer, intent(inout) :: str_len
		character(1), intent(inout) :: str_char
		str_char = co_str(1:1)[im]
		str_len = len(co_str)
	end subroutine sub1

	subroutine sub2(co_str1, co_str2, concate_str, im1, im2)
		integer, intent(in) :: im1, im2
		character(10), intent(in) :: co_str1[*]
		character(100), intent(in) :: co_str2[*]
		character(110), intent(inout) :: concate_str[*]
		concate_str[im1] = co_str1[im2] // co_str2[im2]
	end subroutine sub2

	function func1(co_str, str_len, str_char, im)
		integer, intent(in) :: im
		character(10), intent(in) :: co_str[*]
		integer, intent(in) :: str_len
		character(1), intent(in) :: str_char
		logical :: func1

		if (str_char /= co_str(1:1)[im] .OR. str_len /= len(co_str)) then
			func1 = .FALSE.
		else
			func1 = .TRUE.
		end if
	end function func1

	function func2(co_str1, co_str2, concate_str, im1, im2)
		integer, intent(in) :: im1, im2
		character(10), intent(in) :: co_str1[*]
		character(100), intent(in) :: co_str2[*]
		character(110), intent(in) :: concate_str[*]
		character(200) :: tmp_str
        logical :: func2

		tmp_str = co_str1[im2] // co_str2[im2]

		if(tmp_str /= concate_str[im1]) then
			func2 = .FALSE.
		else
			func2 = .TRUE.
		end if
	end function func2

end program dummyArg001

!!! ************  external subroutine and function  ************ !!!

subroutine sub3(co_str1, co_str2, concate_str, im)
	integer :: im
	character(100), intent(in) :: co_str1[*], co_str2[*]
	character(200), intent(inout) :: concate_str
	concate_str = co_str1[im] // co_str2[im]
end subroutine sub3

function func3(co_str1, co_str2, concate_str, im)
	integer :: im
	character(100), intent(in) :: co_str1[*], co_str2[*]
	character(200), intent(in) :: concate_str
	character(200) :: tmp_str
    logical :: func3

	tmp_str = co_str1[im] // co_str2[im]

	if(tmp_str /= concate_str) then
		func3 = .FALSE.
	else
		func3 = .TRUE.
	end if
end function func3

