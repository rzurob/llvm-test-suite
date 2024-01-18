!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : valueAttrDiffCharArg005.f
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
!*				   and substrings. All of which are 0 byte
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

program valueAttrDiffCharArg005

    character (len=0) :: ch(50)
    character (len=0) :: char1

    ch = ''
    char1 = ''

    call testV(ch, ch(50), ch(16), '', char1(1:0))
    if (ch(50) /= '') error stop 1_4
    if (ch(16) /= '') error stop 2_4
    if (char1 /= '') error stop 3_4

    ch = ''

    call test1(ch, ch(1), ch(27), '', char1(1:0))
    if (ch(1) /= '') error stop 4_4
    if (ch(27) /= '') error stop 5_4
    if (char1 /= '') error stop 6_4

    ch = ''

    call test2(ch, ch(1), ch(38), '', char1(1:0))
    if (ch(1) /= '') error stop 7_4
    if (ch(38) /= '') error stop 8_4
    if (ch(50) /= '') error stop 9_4
    if (char1 /= '') error stop 10_4

    contains

    subroutine test1(tmp, tmp_e1, tmp_e2, lit_arg, subStr)

        character(0) :: tmp(50)
	character(0), VALUE :: tmp_e1, tmp_e2, lit_arg, subStr

	if (lit_arg /= '') error stop 91_4
	if (subStr /= '') error stop 81_4
	tmp_e1 = ''
	if (tmp(1) /= '') error stop 71_4
        tmp = ''
	if (tmp_e1 /= '') error stop 61_4
        if (tmp_e2 /= '') error stop 51_4
	tmp_e2 = ''
	if (tmp(27) /= '') error stop 41_4
	subStr = ''

    end subroutine

    subroutine test2(tmp, tmp_e1, tmp_e2, lit_arg, subStr)

        character(0) :: tmp(50)
	character(0), VALUE :: tmp_e1, tmp_e2, lit_arg, subStr

	if (lit_arg /= '') error stop 92_4
	if (subStr /= '') error stop 82_4
	tmp_e1 = ''
	if (tmp(1) /= '') error stop 72_4
        tmp = ''
	if (tmp_e1 /= '') error stop 62_4
        if (tmp_e2 /= '') error stop 52_4
	tmp_e2 = ''
	if (tmp(38) /= '') error stop 42_4
	subStr = ''

    end subroutine

    subroutine testV(tmp, tmp_e1, tmp_e2, lit_arg, subStr)

        character(0) :: tmp(50)
	character(0) :: tmp_e1, tmp_e2, lit_arg, subStr
	VALUE :: tmp_e1, tmp_e2, lit_arg, subStr

	if (lit_arg /= '') error stop 94_4
	if (subStr /= '') error stop 84_4
	tmp_e1 = ''
	if (tmp(50) /= '') error stop 74_4
        tmp = ''
	if (tmp_e1 /= '') error stop 64_4
        if (tmp_e2 /= '') error stop 54_4
	tmp_e2 = ''
	if (tmp(16) /= '') error stop 43_4
	subStr = ''

    end subroutine

   end
