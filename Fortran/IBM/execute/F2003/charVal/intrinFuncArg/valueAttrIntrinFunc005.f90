!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : valueAttrIntrinFunc005.f
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
!*  DESCRIPTION                : Pass results of intrinsic functions
!*				to subroutines which contain dummy args
!*				using the value attribute. Results will be
!*				0 byte characters
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


program valueAttrIntrinFunc005

    character (len=0) :: maxxVal, minnVal, repVal, minAVal, maxAVal
    character (len=0) :: charArray(10)

    charArray = ''
    charArray(4) = ''

    maxxVal = MAX('', '', '')
    minnVal = MIN('', '', '')
    minAVal = MINVAL(charArray)
    maxAVal = MAXVAL(charArray)
    repVal = REPEAT('', 4)

    call testV(MAX('', '', ''), MIN('', '', ''), REPEAT('', 4), MAXVAL(charArray), MINVAL(charArray))

    call test1(MAX('', '', ''), MIN('', '', ''), REPEAT('', 4), MAXVAL(charArray), MINVAL(charArray))

    call test2(MAX('', '', ''), MIN('', '', ''), REPEAT('', 4), MAXVAL(charArray), MINVAL(charArray))

    call test3(MAX('', '', ''), MIN('', '', ''), REPEAT('', 4), MAXVAL(charArray), MINVAL(charArray))

    contains

    subroutine test1(tmp, tmp2, tmp3, tmp4, tmp5)

        character(0), VALUE :: tmp, tmp2, tmp3, tmp4, tmp5
        if (tmp /= '') error stop 6_4
        if (tmp2 /= '') error stop 7_4
        if (tmp3 /= '') error stop 0_4
        if (tmp4 /= '') error stop 9_4
        if (tmp5 /= '') error stop 10_4

        tmp = ''
        tmp2 = ''
        tmp3 = ''
        tmp4 = ''
        tmp5 = ''

        if (tmp /= '') error stop 26_4
        if (tmp2 /= '') error stop 27_4
        if (tmp3 /= '') error stop 28_4
        if (tmp4 /= '') error stop 29_4
        if (tmp5 /= '') error stop 30_4

    end subroutine

    subroutine test2(tmp, tmp2, tmp3, tmp4, tmp5)

        character(0), VALUE :: tmp, tmp2, tmp3, tmp4, tmp5
        if (tmp /= '') error stop 11_4
        if (tmp2 /= '') error stop 12_4
        if (tmp3 /= '') error stop 13_4   
        if (tmp4 /= '') error stop 14_4
        if (tmp5 /= '') error stop 15_4

        tmp = ''
        tmp2 = ''
        tmp3 = ''
        tmp4 = ''
        tmp5 = ''

        if (tmp /= '') error stop 31_4
        if (tmp2 /= '') error stop 32_4
        if (tmp3 /= '') error stop 33_4
        if (tmp4 /= '') error stop 34_4
        if (tmp5 /= '') error stop 35_4

    end subroutine

    subroutine test3(tmp, tmp2, tmp3, tmp4, tmp5)

        character(0) :: tmp, tmp2, tmp3, tmp4, tmp5
        if (tmp /= '') error stop 16_4
        if (tmp2 /= '') error stop 17_4
        if (tmp3 /= '') error stop 18_4
        if (tmp4 /= '') error stop 19_4
        if (tmp5 /= '') error stop 20_4

        tmp = ''
        tmp2 = ''
        tmp3 = ''
        tmp4 = ''
        tmp5 = ''

        if (tmp /= '') error stop 36_4
        if (tmp2 /= '') error stop 37_4
        if (tmp3 /= '') error stop 38_4
        if (tmp4 /= '') error stop 39_4
        if (tmp5 /= '') error stop 40_4

    end subroutine

    subroutine testV(tmp, tmp2, tmp3, tmp4, tmp5)

        character(0) :: tmp, tmp2, tmp3, tmp4, tmp5
	VALUE :: tmp, tmp2, tmp3, tmp4, tmp5
        if (tmp /= '') error stop 1_4
        if (tmp2 /= '') error stop 2_4
        if (tmp3 /= '') error stop 3_4
        if (tmp4 /= '') error stop 4_4
        if (tmp5 /= '') error stop 5_4

        tmp = ''
        tmp2 = ''
        tmp3 = ''
        tmp4 = ''
        tmp5 = ''

        if (tmp /= '') error stop 21_4
        if (tmp2 /= '') error stop 22_4
        if (tmp3 /= '') error stop 23_4
        if (tmp4 /= '') error stop 24_4
        if (tmp5 /= '') error stop 25_4

    end subroutine

   end
