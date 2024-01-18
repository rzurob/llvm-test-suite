!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : valueAttrIntrinFunc001.f
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
!*				8 byte characters
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


program valueAttrIntrinFunc001

    character (len=8) :: maxxVal, minnVal, repVal, minAVal, maxAVal
    character (len=8) :: charArray(10)

    charArray = 'abcdefgh'
    charArray(4) = 'zzyxqwop'

    maxxVal = MAX('abcdefgh', 'bbcddset', 'zzyxqwop')
    minnVal = MIN('abcdefgh', 'bbcddset', 'zzyxqwop')
    minAVal = MINVAL(charArray)
    maxAVal = MAXVAL(charArray)
    repVal = REPEAT('ab', 4)

    call testV(MAX('abcdefgh', 'bbcddset', 'zzyxqwop'), MIN('abcdefgh', 'bbcddset', 'zzyxqwop'), REPEAT('ab', 4), MAXVAL(charArray), MINVAL(charArray))

    call test1(MAX('abcdefgh', 'bbcddset', 'zzyxqwop'), MIN('abcdefgh', 'bbcddset', 'zzyxqwop'), REPEAT('ab', 4), MAXVAL(charArray), MINVAL(charArray))

    call test2(MAX('abcdefgh', 'bbcddset', 'zzyxqwop'), MIN('abcdefgh', 'bbcddset', 'zzyxqwop'), REPEAT('ab', 4), MAXVAL(charArray), MINVAL(charArray))

    call test3(MAX('abcdefgh', 'bbcddset', 'zzyxqwop'), MIN('abcdefgh', 'bbcddset', 'zzyxqwop'), REPEAT('ab', 4), MAXVAL(charArray), MINVAL(charArray))

    contains

    subroutine test1(tmp, tmp2, tmp3, tmp4, tmp5)

        character(8), VALUE :: tmp, tmp2, tmp3, tmp4, tmp5
        if (tmp /= 'zzyxqwop') error stop 6_4
        if (tmp2 /= 'abcdefgh') error stop 7_4
        if (tmp3 /= 'abababab') error stop 8_4
        if (tmp4 /= 'zzyxqwop') error stop 9_4
        if (tmp5 /= 'abcdefgh') error stop 10_4

        tmp = 'xxxxxxxx'
        tmp2 = 'xxxxxxxx'
        tmp3 = 'xxxxxxxx'
        tmp4 = 'xxxxxxxx'
        tmp5 = 'xxxxxxxx'

        if (tmp /= 'xxxxxxxx') error stop 26_4
        if (tmp2 /= 'xxxxxxxx') error stop 27_4
        if (tmp3 /= 'xxxxxxxx') error stop 28_4
        if (tmp4 /= 'xxxxxxxx') error stop 29_4
        if (tmp5 /= 'xxxxxxxx') error stop 30_4

    end subroutine

    subroutine test2(tmp, tmp2, tmp3, tmp4, tmp5)

        character(7), VALUE :: tmp, tmp2, tmp3, tmp4, tmp5
	if (tmp /= 'zzyxqwo') error stop 11_4
        if (tmp2 /= 'abcdefg') error stop 12_4
        if (tmp3 /= 'abababa') error stop 13_4   
        if (tmp4 /= 'zzyxqwo') error stop 14_4
        if (tmp5 /= 'abcdefg') error stop 15_4
        
	tmp = 'xxxxxxx'
        tmp2 = 'xxxxxxx'
        tmp3 = 'xxxxxxx'
        tmp4 = 'xxxxxxx'
        tmp5 = 'xxxxxxx'

        if (tmp /= 'xxxxxxx') error stop 31_4
        if (tmp2 /= 'xxxxxxx') error stop 32_4
        if (tmp3 /= 'xxxxxxx') error stop 33_4
        if (tmp4 /= 'xxxxxxx') error stop 34_4
        if (tmp5 /= 'xxxxxxx') error stop 35_4

    end subroutine

    subroutine test3(tmp, tmp2, tmp3, tmp4, tmp5)

        character(8) :: tmp, tmp2, tmp3, tmp4, tmp5
        if (tmp /= 'zzyxqwop') error stop 16_4
        if (tmp2 /= 'abcdefgh') error stop 17_4
        if (tmp3 /= 'abababab') error stop 18_4
        if (tmp4 /= 'zzyxqwop') error stop 19_4
        if (tmp5 /= 'abcdefgh') error stop 20_4

        tmp = 'xxxxxxxx'
        tmp2 = 'xxxxxxxx'
        tmp3 = 'xxxxxxxx'
        tmp4 = 'xxxxxxxx'
        tmp5 = 'xxxxxxxx'

        if (tmp /= 'xxxxxxxx') error stop 36_4
        if (tmp2 /= 'xxxxxxxx') error stop 37_4
        if (tmp3 /= 'xxxxxxxx') error stop 38_4
        if (tmp4 /= 'xxxxxxxx') error stop 39_4
        if (tmp5 /= 'xxxxxxxx') error stop 40_4

    end subroutine

    subroutine testV(tmp, tmp2, tmp3, tmp4, tmp5)

        character(8) :: tmp, tmp2, tmp3, tmp4, tmp5
	VALUE :: tmp, tmp2, tmp3, tmp4, tmp5
	if (tmp /= 'zzyxqwop') error stop 1_4
        if (tmp2 /= 'abcdefgh') error stop 2_4
        if (tmp3 /= 'abababab') error stop 3_4
        if (tmp4 /= 'zzyxqwop') error stop 4_4
        if (tmp5 /= 'abcdefgh') error stop 5_4

        tmp = 'xxxxxxxx'
        tmp2 = 'xxxxxxxx'
        tmp3 = 'xxxxxxxx'
        tmp4 = 'xxxxxxxx'
        tmp5 = 'xxxxxxxx'

        if (tmp /= 'xxxxxxxx') error stop 21_4
        if (tmp2 /= 'xxxxxxxx') error stop 22_4
        if (tmp3 /= 'xxxxxxxx') error stop 23_4
        if (tmp4 /= 'xxxxxxxx') error stop 24_4
        if (tmp5 /= 'xxxxxxxx') error stop 25_4

    end subroutine

   end
