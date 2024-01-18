!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : valueAttrDiffCharArg003.f
!*
!*  PROGRAMMER                 : Vicram Uppal
!*  DATE                       : 04/03/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : The following was passed as actual args
!*				to subroutines which contain the value 
!*				attribute with its respective dummy args:
!*				   literals, array, elements of an array,
!*				   and substrings. All of which are 128 byte
!*				   character strings
!*
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

program valueAttrDiffCharArg003

    character (len=128) :: ch(50)
    character (len=160) :: char1
      
    ch = & 
    'aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd'
    
    char1 = &
    'lfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxt'
    
    call testV(ch, ch(50), ch(16), &
    'qwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyui', &
    char1(32:159))

    if (ch(50) /= &
    'xyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxy') &
    error stop 1_4
    
    if (ch(16) /= &
    'xyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxy') &
    error stop 2_4

    if (char1 /= &
    'lfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxt') &
    error stop 3_4

    ch = &
    'aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd'

    call test1(ch, ch(1), ch(27), &
    'qwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyui', &
    char1(32:159))

    if (ch(1) /= &
    'xyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxy') &
    error stop 4_4

    if (ch(27) /= &
    'xyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxy') &
    error stop 5_4

    if (char1 /= &
    'lfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxt') &
    error stop 6_4

    ch = &
    'aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd'

    call test2(ch, ch(1), ch(38), &
    'qwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyui', &
    char1(32:159))

    if (ch(1) /= &
    'xyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxx') &
    error stop 7_4

    if (ch(38) /= &
    'zxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyz') &
    error stop 8_4

    if (ch(50) /= &
    'yzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd') &
    error stop 9_4

    if (char1 /= &
    'lfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxt') &
    error stop 10_4

    ch = &
    'aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd'

    contains

    subroutine test1(tmp, tmp_e1, tmp_e2, lit_arg, subStr)

        character(128) :: tmp(50)
	character(128), VALUE :: tmp_e1, tmp_e2, lit_arg, subStr

	if (lit_arg /= &
	'qwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyui') &
	error stop 91_4

	if (subStr /= &
	'ubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedx') &
	error stop 81_4

	tmp_e1 = &
	'aaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbb'

	if (tmp(1) /= &
	'aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd') &
	error stop 71_4

        tmp = &
	'xyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxy'

	if (tmp_e1 /= &
	'aaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbb') &
	error stop 61_4

        if (tmp_e2 /= &
	'aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd') &
	error stop 51_4

	tmp_e2 = &
	'yyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzz'

	if (tmp(27) /= &
	'xyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxy') &
	error stop 41_4

	subStr = &
	'tttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt'

    end subroutine

    subroutine test2(tmp, tmp_e1, tmp_e2, lit_arg, subStr)

        character(127) :: tmp(50)
	character(127), VALUE :: tmp_e1, tmp_e2, lit_arg, subStr

	if (lit_arg /= &
	'qwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyu') &
	error stop 92_4

	if (subStr /= &
	'ubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyued') &
	error stop 82_4

	tmp_e1 = &
	'hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh'

	if (tmp(1) /= &
	'aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccd') &
	error stop 72_4

        tmp = &
	'xyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxy'

	if (tmp_e1 /= &
	'hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh') &
	error stop 62_4

        if (tmp_e2 /= &
	'aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccd') &
	error stop 52_4

	tmp_e2 = &
	'yyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzz'

	if (tmp(38) /= &
	'xyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzx') &
	error stop 42_4

	subStr = &
	'ttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt'

    end subroutine

    subroutine testV(tmp, tmp_e1, tmp_e2, lit_arg, subStr)

        character(128) :: tmp(50)
	character(128) :: tmp_e1, tmp_e2, lit_arg, subStr
	VALUE :: tmp_e1, tmp_e2, lit_arg, subStr
	if (lit_arg /= &
	'qwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyuiqwertyui') &
	error stop 94_4

	if (subStr /= &
	'ubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedxtlfhdviuwapmubfyuedx') &
	error stop 84_4

	tmp_e1 = &
	'aaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbb'

	if (tmp(50) /= &
	'aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd') &
	error stop 74_4

	tmp = &
	'xyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxy'

	if (tmp_e1 /= &
	'aaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbbaaaabbbb') &
	error stop 64_4

        if (tmp_e2 /= &
	'aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd') &
	error stop 54_4

	tmp_e2 = &
	'yyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzzyyyyzzzz'

	if (tmp(16) /= &
	'xyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxyxyzxyzxy') &
	error stop 43_4

	subStr = &
	'tttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt'

    end subroutine

   end
