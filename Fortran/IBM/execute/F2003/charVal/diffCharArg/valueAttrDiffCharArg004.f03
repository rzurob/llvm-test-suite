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
!*				   and substrings. All of which are 256 byte
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

program valueAttrDiffCharArg004

    character (len=256) :: ch(50)
    character (len=320) :: char1

    ch = &
    'aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd'

    char1 = &
    'lfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxt'

    call testV(ch, ch(50), ch(16), &
    'qwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyui', &
    char1(64:319))

    if (ch(50) /= &
    'xyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxy') &
    error stop 1_4

    if (ch(16) /= &
    'xyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxy') &
    error stop 2_4

    if (char1 /= &
    'lfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxt') &
    error stop 3_4

    ch = &
    'aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd'

    call test1(ch, ch(1), ch(27), &
    'qwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyui', &
    char1(64:319))

    if (ch(1) /= &
    'xyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxy') &
    error stop 4_4

    if (ch(27) /= &
    'xyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxy') &
    error stop 5_4

    if (char1 /= &
    'lfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxt') &
    error stop 6_4

    ch = &
    'aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd'

    call test2(ch, ch(1), ch(38), &
    'qwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyui', &
    char1(64:319))

    if (ch(1) /= &
    'xyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxx') &
    error stop 7_4

    if (ch(38) /= &
    'zxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyz') &
    error stop 8_4

    if (ch(50) /= &
    'yzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd') &
    error stop 9_4

    if (char1 /= &
    'lfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxt') &
    error stop 10_4

    ch = &
    'aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd'

    contains

    subroutine test1(tmp, tmp_e1, tmp_e2, lit_arg, subStr)

        character(256) :: tmp(50)
	character(256), VALUE :: tmp_e1, tmp_e2, lit_arg, subStr

	if (lit_arg /= &
	'qwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyui') &
	error stop 91_4

	if (subStr /= &
	'dviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedx') &
	error stop 81_4

	tmp_e1 = &
	'aaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbb'

	if (tmp(1) /= &
	'aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd') &
	error stop 71_4

        tmp = &
	'xyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxy'

	if (tmp_e1 /= &
	'aaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbb') &
	error stop 61_4

        if (tmp_e2 /= &
	'aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd') &
	error stop 51_4

	tmp_e2 = &
	'yyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzz'

	if (tmp(27) /= &
	'xyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxy') &
	error stop 41_4

	subStr = &
	'tttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt'

    end subroutine

    subroutine test2(tmp, tmp_e1, tmp_e2, lit_arg, subStr)

        character(255) :: tmp(50)
	character(255), VALUE :: tmp_e1, tmp_e2, lit_arg, subStr

	if (lit_arg /= &
	'qwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyu') &
	error stop 92_4

	if (subStr /= &
	'dviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyued') &
	error stop 82_4

	tmp_e1 = &
	'hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh'

	if (tmp(1) /= &
	'aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccd') &
	error stop 72_4

        tmp = &
	'xyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxy'

	if (tmp_e1 /= &
	'hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh') &
	error stop 62_4

        if (tmp_e2 /= &
	'aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccd') &
	error stop 52_4

	tmp_e2 = &
	'yyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzz'

	if (tmp(38) /= &
	'xyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzx') &
	error stop 42_4

	subStr = &
	'tttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt'

    end subroutine

    subroutine testV(tmp, tmp_e1, tmp_e2, lit_arg, subStr)

        character(256) :: tmp(50)
	character(256) :: tmp_e1, tmp_e2, lit_arg, subStr
	VALUE :: tmp_e1, tmp_e2, lit_arg, subStr
	if (lit_arg /= &
	'qwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyui') &
	error stop 94_4

	if (subStr /= &
	'dviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedx') &
	error stop 84_4

	tmp_e1 = &
	'aaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbb'

	if (tmp(50) /= &
	'aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd') &
	error stop 74_4

	tmp = &
	'xyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxy'

	if (tmp_e1 /= &
	'aaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbb') &
	error stop 64_4

        if (tmp_e2 /= &
	'aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd') &
	error stop 54_4

	tmp_e2 = &
	'yyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzz'

	if (tmp(16) /= &
	'xyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxy') &
	error stop 43_4

	subStr = &
	'tttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt'

    end subroutine

   end
