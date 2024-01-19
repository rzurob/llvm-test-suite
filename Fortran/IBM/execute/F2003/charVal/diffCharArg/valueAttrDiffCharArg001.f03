!*  ===================================================================
!*
!*  DATE                       : 04/03/2006
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : The following was passed as actual args
!*				to subroutines which contain the value
!*				attribute with its respective dummy args:
!*				   literals, array, elements of an array,
!*				   and substrings. All of which are 8 byte
!*				   character strings
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program valueAttrDiffCharArg001

    character (len=8) :: ch(50)
    character (len=20) :: char1

    ch = 'aabbccdd'
    char1 = 'lfhdviuwapmubfyuedxt'

    call testV(ch, ch(50), ch(16), 'qwertyui', char1(5:12))
    if (ch(50) /= 'xyzxyzxy') error stop 1_4
    if (ch(16) /= 'xyzxyzxy') error stop 2_4
    if (char1 /= 'lfhdviuwapmubfyuedxt') error stop 3_4

    ch = 'aabbccdd'

    call test1(ch, ch(1), ch(27), 'qwertyui', char1(5:12))
    if (ch(1) /= 'xyzxyzxy') error stop 4_4
    if (ch(27) /= 'xyzxyzxy') error stop 5_4
    if (char1 /= 'lfhdviuwapmubfyuedxt') error stop 6_4

    ch = 'aabbccdd'

    call test2(ch, ch(1), ch(38), 'qwertyui', char1(5:12))
    if (ch(1) /= 'xyzxyzxx') error stop 7_4
    if (ch(38) /= 'zxyzxxyz') error stop 8_4
    if (ch(50) /= 'aabbccdd') error stop 9_4
    if (char1 /= 'lfhdviuwapmubfyuedxt') error stop 10_4

    contains

    subroutine test1(tmp, tmp_e1, tmp_e2, lit_arg, subStr)

        character(8) :: tmp(50)
	character(8), VALUE :: tmp_e1, tmp_e2, lit_arg, subStr

	if (lit_arg /= 'qwertyui') error stop 91_4
	if (subStr /= 'viuwapmu') error stop 81_4
	tmp_e1 = 'aaaabbbb'
	if (tmp(1) /= 'aabbccdd') error stop 71_4
        tmp = 'xyzxyzxy'
	if (tmp_e1 /= 'aaaabbbb') error stop 61_4
        if (tmp_e2 /= 'aabbccdd') error stop 51_4
	tmp_e2 = 'yyyyzzzz'
	if (tmp(27) /= 'xyzxyzxy') error stop 41_4
	subStr = 'tttttttt'

    end subroutine

    subroutine test2(tmp, tmp_e1, tmp_e2, lit_arg, subStr)

        character(7) :: tmp(50)
	character(7), VALUE :: tmp_e1, tmp_e2, lit_arg, subStr

	if (lit_arg /= 'qwertyu') error stop 92_4
	if (subStr /= 'viuwapm') error stop 82_4
	tmp_e1 = 'hhhhhhhh'
	if (tmp(1) /= 'aabbccd') error stop 72_4
        tmp = 'xyzxyzxy'
	if (tmp_e1 /= 'hhhhhhh') error stop 62_4
        if (tmp_e2 /= 'aabbccd') error stop 52_4
	tmp_e2 = 'yyyyzzzz'
	if (tmp(38) /= 'xyzxyzx') error stop 42_4
	subStr = 'tttttttt'

    end subroutine

    subroutine testV(tmp, tmp_e1, tmp_e2, lit_arg, subStr)

        character(8) :: tmp(50)
	character(8) :: tmp_e1, tmp_e2, lit_arg, subStr
	VALUE :: tmp_e1, tmp_e2, lit_arg, subStr

	if (lit_arg /= 'qwertyui') error stop 94_4
	if (subStr /= 'viuwapmu') error stop 84_4
	tmp_e1 = 'aaaabbbb'
	if (tmp(50) /= 'aabbccdd') error stop 74_4
        tmp = 'xyzxyzxy'
	if (tmp_e1 /= 'aaaabbbb') error stop 64_4
        if (tmp_e2 /= 'aabbccdd') error stop 54_4
	tmp_e2 = 'yyyyzzzz'
	if (tmp(16) /= 'xyzxyzxy') error stop 43_4
	subStr = 'tttttttt'

    end subroutine

   end
