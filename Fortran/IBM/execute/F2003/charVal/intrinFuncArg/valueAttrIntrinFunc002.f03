!*  ===================================================================
!*
!*  DATE                       : 04/03/2006
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Pass results of intrinsic functions
!*				to subroutines which contain dummy args
!*				using the value attribute. Results will be
!*				16 byte characters
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


program valueAttrIntrinFunc002

    character (len=16) :: maxxVal, minnVal, repVal, minAVal, maxAVal
    character (len=16) :: charArray(10)

    charArray = 'abcdefghabcdefgh'
    charArray(8) = 'zzyxqwopzzyxqwop'

    maxxVal = MAX('abcdefgh', 'bbcddset', 'zzyxqwop')
    minnVal = MIN('abcdefgh', 'bbcddset', 'zzyxqwop')
    minAVal = MINVAL(charArray)
    maxAVal = MAXVAL(charArray)
    repVal = REPEAT('ab', 4)

    call testV(MAX('abcdefghabcdefgh', 'bbcddsetbbcddset', 'zzyxqwopzzyxqwop'), MIN('abcdefghabcdefgh', 'bbcddsetbbcddset', 'zzyxqwopzzyxqwop'), REPEAT('abab', 4), MAXVAL(charArray), MINVAL(charArray))

    call test1(MAX('abcdefghabcdefgh', 'bbcddsetbbcddset', 'zzyxqwopzzyxqwop'), MIN('abcdefghabcdefgh', 'bbcddsetbbcddset', 'zzyxqwopzzyxqwop'), REPEAT('abab', 4), MAXVAL(charArray), MINVAL(charArray))

    call test2(MAX('abcdefghabcdefgh', 'bbcddsetbbcddset', 'zzyxqwopzzyxqwop'), MIN('abcdefghabcdefgh', 'bbcddsetbbcddset', 'zzyxqwopzzyxqwop'), REPEAT('abab', 4), MAXVAL(charArray), MINVAL(charArray))

    call test3(MAX('abcdefghabcdefgh', 'bbcddsetbbcddset', 'zzyxqwopzzyxqwop'), MIN('abcdefghabcdefgh', 'bbcddsetbbcddset', 'zzyxqwopzzyxqwop'), REPEAT('abab', 4), MAXVAL(charArray), MINVAL(charArray))

    contains

    subroutine test1(tmp, tmp2, tmp3, tmp4, tmp5)

        character(16), VALUE :: tmp, tmp2, tmp3, tmp4, tmp5
        if (tmp /= 'zzyxqwopzzyxqwop') error stop 6_4
        if (tmp2 /= 'abcdefghabcdefgh') error stop 7_4
        if (tmp3 /= 'abababababababab') error stop 8_4
        if (tmp4 /= 'zzyxqwopzzyxqwop') error stop 9_4
        if (tmp5 /= 'abcdefghabcdefgh') error stop 10_4

        tmp = 'xxxxxxxxxxxxxxxx'
        tmp2 = 'xxxxxxxxxxxxxxxx'
        tmp3 = 'xxxxxxxxxxxxxxxx'
        tmp4 = 'xxxxxxxxxxxxxxxx'
        tmp5 = 'xxxxxxxxxxxxxxxx'

        if (tmp /= 'xxxxxxxxxxxxxxxx') error stop 26_4
        if (tmp2 /= 'xxxxxxxxxxxxxxxx') error stop 27_4
        if (tmp3 /= 'xxxxxxxxxxxxxxxx') error stop 28_4
        if (tmp4 /= 'xxxxxxxxxxxxxxxx') error stop 29_4
        if (tmp5 /= 'xxxxxxxxxxxxxxxx') error stop 30_4

    end subroutine

    subroutine test2(tmp, tmp2, tmp3, tmp4, tmp5)

        character(15), VALUE :: tmp, tmp2, tmp3, tmp4, tmp5
        if (tmp /= 'zzyxqwopzzyxqwo') error stop 11_4
        if (tmp2 /= 'abcdefghabcdefg') error stop 12_4
        if (tmp3 /= 'abababababababa') error stop 13_4
        if (tmp4 /= 'zzyxqwopzzyxqwo') error stop 14_4
        if (tmp5 /= 'abcdefghabcdefg') error stop 15_4

        tmp = 'xxxxxxxxxxxxxxxx'
        tmp2 = 'xxxxxxxxxxxxxxxx'
        tmp3 = 'xxxxxxxxxxxxxxxx'
        tmp4 = 'xxxxxxxxxxxxxxxx'
        tmp5 = 'xxxxxxxxxxxxxxxx'

        if (tmp /= 'xxxxxxxxxxxxxxx') error stop 31_4
        if (tmp2 /= 'xxxxxxxxxxxxxxx') error stop 32_4
        if (tmp3 /= 'xxxxxxxxxxxxxxx') error stop 33_4
        if (tmp4 /= 'xxxxxxxxxxxxxxx') error stop 34_4
        if (tmp5 /= 'xxxxxxxxxxxxxxx') error stop 35_4
    end subroutine

    subroutine test3(tmp, tmp2, tmp3, tmp4, tmp5)

        character(16) :: tmp, tmp2, tmp3, tmp4, tmp5
        if (tmp /= 'zzyxqwopzzyxqwop') error stop 16_4
        if (tmp2 /= 'abcdefghabcdefgh') error stop 17_4
        if (tmp3 /= 'abababababababab') error stop 18_4
        if (tmp4 /= 'zzyxqwopzzyxqwop') error stop 19_4
        if (tmp5 /= 'abcdefghabcdefgh') error stop 20_4

        tmp = 'xxxxxxxxxxxxxxxx'
        tmp2 = 'xxxxxxxxxxxxxxxx'
        tmp3 = 'xxxxxxxxxxxxxxxx'
        tmp4 = 'xxxxxxxxxxxxxxxx'
        tmp5 = 'xxxxxxxxxxxxxxxx'

        if (tmp /= 'xxxxxxxxxxxxxxxx') error stop 36_4
        if (tmp2 /= 'xxxxxxxxxxxxxxxx') error stop 37_4
        if (tmp3 /= 'xxxxxxxxxxxxxxxx') error stop 38_4
        if (tmp4 /= 'xxxxxxxxxxxxxxxx') error stop 39_4
        if (tmp5 /= 'xxxxxxxxxxxxxxxx') error stop 40_4

    end subroutine

    subroutine testV(tmp, tmp2, tmp3, tmp4, tmp5)

        character(16) :: tmp, tmp2, tmp3, tmp4, tmp5
	VALUE :: tmp, tmp2, tmp3, tmp4, tmp5
        if (tmp /= 'zzyxqwopzzyxqwop') error stop 1_4
        if (tmp2 /= 'abcdefghabcdefgh') error stop 2_4
        if (tmp3 /= 'abababababababab') error stop 3_4
        if (tmp4 /= 'zzyxqwopzzyxqwop') error stop 4_4
        if (tmp5 /= 'abcdefghabcdefgh') error stop 5_4

        tmp = 'xxxxxxxxxxxxxxxx'
        tmp2 = 'xxxxxxxxxxxxxxxx'
        tmp3 = 'xxxxxxxxxxxxxxxx'
        tmp4 = 'xxxxxxxxxxxxxxxx'
        tmp5 = 'xxxxxxxxxxxxxxxx'

        if (tmp /= 'xxxxxxxxxxxxxxxx') error stop 21_4
        if (tmp2 /= 'xxxxxxxxxxxxxxxx') error stop 22_4
        if (tmp3 /= 'xxxxxxxxxxxxxxxx') error stop 23_4
        if (tmp4 /= 'xxxxxxxxxxxxxxxx') error stop 24_4
        if (tmp5 /= 'xxxxxxxxxxxxxxxx') error stop 25_4

    end subroutine

   end
