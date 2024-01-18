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
!*				   and substrings. All of which are 16 byte
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

program valueAttrDiffCharArg002

    character (len=16) :: ch(50)
    character (len=20) :: char1

    ch = 'aabbccddaabbccdd'
    char1 = 'lfhdviuwapmubfyuedxt'

    call testV(ch, ch(50), ch(16), 'qwertyuiqwertyui', char1(4:19))
    if (ch(50) /= 'xyzxyzxyxyzxyzxy') error stop 1_4
    if (ch(16) /= 'xyzxyzxyxyzxyzxy') error stop 2_4
    if (char1 /= 'lfhdviuwapmubfyuedxt') error stop 3_4

    ch = 'aabbccddaabbccdd'

    call test1(ch, ch(1), ch(27), 'qwertyuiqwertyui', char1(4:19))
    if (ch(1) /= 'xyzxyzxyxyzxyzxy') error stop 4_4
    if (ch(27) /= 'xyzxyzxyxyzxyzxy') error stop 5_4
    if (char1 /= 'lfhdviuwapmubfyuedxt') error stop 6_4

    ch = 'aabbccddaabbccdd'

    call test2(ch, ch(1), ch(38), 'qwertyuiqwertyui', char1(4:19))
    if (ch(1) /= 'xyzxyzxyxyzxyzxx') error stop 7_4
    if (ch(38) /= 'yxyzxyzxxyzxyzxy') error stop 8_4
    if (ch(50) /= 'aabbccddaabbccdd') error stop 9_4
    if (char1 /= 'lfhdviuwapmubfyuedxt') error stop 10_4

    contains

    subroutine test1(tmp, tmp_e1, tmp_e2, lit_arg, subStr)

        character(16) :: tmp(50)
	character(16), VALUE :: tmp_e1, tmp_e2, lit_arg, subStr

	if (lit_arg /= 'qwertyuiqwertyui') error stop 91_4
	if (subStr /= 'dviuwapmubfyuedx') error stop 81_4
	tmp_e1 = 'aaaabbbbaaaabbbb'
	if (tmp(1) /= 'aabbccddaabbccdd') error stop 71_4
        tmp = 'xyzxyzxyxyzxyzxy'
	if (tmp_e1 /= 'aaaabbbbaaaabbbb') error stop 61_4
        if (tmp_e2 /= 'aabbccddaabbccdd') error stop 51_4
	tmp_e2 = 'yyyyzzzzyyyyzzzz'
	if (tmp(27) /= 'xyzxyzxyxyzxyzxy') error stop 41_4
	subStr = 'tttttttttttttttt'

    end subroutine

    subroutine test2(tmp, tmp_e1, tmp_e2, lit_arg, subStr)

        character(15) :: tmp(50)
	character(15), VALUE :: tmp_e1, tmp_e2, lit_arg, subStr

	if (lit_arg /= 'qwertyuiqwertyu') error stop 92_4
	if (subStr /= 'dviuwapmubfyued') error stop 82_4
	tmp_e1 = 'hhhhhhhhhhhhhhhh'
	if (tmp(1) /= 'aabbccddaabbccd') error stop 72_4
        tmp = 'xyzxyzxyxyzxyzxy'
	if (tmp_e1 /= 'hhhhhhhhhhhhhhh') error stop 62_4
        if (tmp_e2 /= 'aabbccddaabbccd') error stop 52_4
	tmp_e2 = 'yyyyzzzzyyyyzzz'
	if (tmp(38) /= 'xyzxyzxyxyzxyzx') error stop 42_4
	subStr = 'ttttttttttttttt'

    end subroutine

    subroutine testV(tmp, tmp_e1, tmp_e2, lit_arg, subStr)

        character(16) :: tmp(50)
	character(16) :: tmp_e1, tmp_e2, lit_arg, subStr
	VALUE :: tmp_e1, tmp_e2, lit_arg, subStr
	if (lit_arg /= 'qwertyuiqwertyui') error stop 94_4
	if (subStr /= 'dviuwapmubfyuedx') error stop 84_4
	tmp_e1 = 'aaaabbbbaaaabbbb'
	if (tmp(50) /= 'aabbccddaabbccdd') error stop 74_4
        tmp = 'xyzxyzxyxyzxyzxy'
	if (tmp_e1 /= 'aaaabbbbaaaabbbb') error stop 64_4
        if (tmp_e2 /= 'aabbccddaabbccdd') error stop 54_4
	tmp_e2 = 'yyyyzzzzyyyyzzzz'
	if (tmp(16) /= 'xyzxyzxyxyzxyzxy') error stop 43_4
	subStr = 'tttttttttttttttt'

    end subroutine

   end
