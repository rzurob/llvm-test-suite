!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : valueAttrIntrinFunc003.f
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
!*				128 byte characters                          
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


program valueAttrIntrinFunc003

    character (len=128) :: maxxVal, minnVal, repVal, minAVal, maxAVal
    character (len=128) :: charArray(10)

    charArray = &
    'abcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefgh'
    charArray(2) = &
    'zzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwop'

    maxxVal = MAX('abcdefgh', 'bbcddset', 'zzyxqwop')
    minnVal = MIN('abcdefgh', 'bbcddset', 'zzyxqwop')
    minAVal = MINVAL(charArray)
    maxAVal = MAXVAL(charArray)
    repVal = REPEAT('ab', 4)

    call testV(MAX('abcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefgh', &
    'bbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddset', &
    'zzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwop'), &
    MIN('abcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefgh', &
    'bbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddset', &
    'zzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwop'), &
    REPEAT('abab', 32), MAXVAL(charArray), MINVAL(charArray))

    call test1(MAX('abcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefgh', &
    'bbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddset', &
    'zzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwop'), &
    MIN('abcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefgh', &
    'bbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddset', &
    'zzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwop'), &
    REPEAT('abab', 32), MAXVAL(charArray), MINVAL(charArray))

    call test2(MAX('abcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefgh', &
    'bbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddset', &
    'zzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwop'), &
    MIN('abcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefgh', &
    'bbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddset', &
    'zzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwop'), &
    REPEAT('abab', 32), MAXVAL(charArray), MINVAL(charArray))

    call test3(MAX('abcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefgh', &
    'bbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddset', &
    'zzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwop'), &
    MIN('abcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefgh', &
    'bbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddset', &
    'zzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwop'), &
    REPEAT('abab', 32), MAXVAL(charArray), MINVAL(charArray))

    contains

    subroutine test1(tmp, tmp2, tmp3, tmp4, tmp5)

        character(128), VALUE :: tmp, tmp2, tmp3, tmp4, tmp5
        if (tmp /= &
        'zzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwop') &
        error stop 6_4
    
        if (tmp2 /= &
        'abcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefgh') &
        error stop 7_4
    
        if (tmp3 /= &
        'abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab') &
        error stop 8_4
    
        if (tmp4 /= &
        'zzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwop') &
        error stop 9_4
    
        if (tmp5 /= &
        'abcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefgh') &
        error stop 10_4

        tmp = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        tmp2 = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        tmp3 = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        tmp4 = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        tmp5 = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
 
         if (tmp /= &
        'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
        error stop 26_4
    
        if (tmp2 /= &
        'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
        error stop 27_4
    
        if (tmp3 /= &
        'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
        error stop 28_4
    
        if (tmp4 /= &
        'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
        error stop 29_4
    
        if (tmp5 /= &
        'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
        error stop 230_4 

    end subroutine

    subroutine test2(tmp, tmp2, tmp3, tmp4, tmp5)

        character(127), VALUE :: tmp, tmp2, tmp3, tmp4, tmp5
        if (tmp /= &
        'zzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwo') &
        error stop 11_4
     
        if (tmp2 /= &
        'abcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefg') &
        error stop 12_4
     
        if (tmp3 /= &
        'abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababa') &
        error stop 13_4   
     
        if (tmp4 /= &
        'zzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwo') &
        error stop 14_4
     
        if (tmp5 /= &
        'abcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefg') &
        error stop 15_4

        tmp = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        tmp2 = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        tmp3 = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        tmp4 = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        tmp5 = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'

        if (tmp /= &
        'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
        error stop 31_4
    
        if (tmp2 /= &
        'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
        error stop 32_4
    
        if (tmp3 /= &
        'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
        error stop 33_4
    
        if (tmp4 /= &
        'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
        error stop 34_4
    
        if (tmp5 /= &
        'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
        error stop 35_4

    end subroutine

    subroutine test3(tmp, tmp2, tmp3, tmp4, tmp5)

        character(128) :: tmp, tmp2, tmp3, tmp4, tmp5
        if (tmp /= &
        'zzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwop') &
        error stop 16_4
    
        if (tmp2 /= &
        'abcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefgh') &
        error stop 17_4
    
        if (tmp3 /= &
        'abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab') &
        error stop 18_4
    
        if (tmp4 /= &
        'zzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwop') &
        error stop 19_4
    
        if (tmp5 /= &
        'abcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefgh') &
        error stop 20_4

        tmp = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx' 
        tmp2 = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        tmp3 = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        tmp4 = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        tmp5 = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'

        if (tmp /= &
        'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
        error stop 36_4
    
        if (tmp2 /= &
        'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
        error stop 37_4
    
        if (tmp3 /= &
        'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
        error stop 38_4
    
        if (tmp4 /= &
        'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
        error stop 39_4
    
        if (tmp5 /= &
        'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
        error stop 40_4

    end subroutine

    subroutine testV(tmp, tmp2, tmp3, tmp4, tmp5)

        character(128) :: tmp, tmp2, tmp3, tmp4, tmp5
	VALUE :: tmp, tmp2, tmp3, tmp4, tmp5

        if (tmp /= &
        'zzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwop') &
        error stop 1_4
    
        if (tmp2 /= &
        'abcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefgh') &
        error stop 2_4
    
        if (tmp3 /= &
        'abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab') &
        error stop 3_4
    
        if (tmp4 /= &
        'zzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwop') &
        error stop 4_4
    
        if (tmp5 /= &
        'abcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefgh') &
        error stop 5_4

        tmp = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx' 
        tmp2 = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        tmp3 = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        tmp4 = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        tmp5 = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'

        if (tmp /= &
        'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
        error stop 21_4
    
        if (tmp2 /= &
        'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
        error stop 22_4
    
        if (tmp3 /= &
        'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
        error stop 23_4
    
        if (tmp4 /= &
        'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
        error stop 24_4
    
        if (tmp5 /= &
        'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
        error stop 25_4

    end subroutine

   end
