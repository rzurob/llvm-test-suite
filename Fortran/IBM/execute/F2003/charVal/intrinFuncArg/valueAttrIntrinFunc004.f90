!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : valueAttrIntrinFunc004.f
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
!*				256 byte characters                          
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


program valueAttrIntrinFunc004

    character (len=256) :: maxxVal, minnVal, repVal, minAVal, maxAVal
    character (len=256) :: charArray(10)

    charArray = &
    'abcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefgh'

    charArray(9) = &
    'zzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwop'

    minAVal = MINVAL(charArray)
    maxAVal = MAXVAL(charArray)
    repVal = REPEAT('abab', 64)

    call testV(MAX('abcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefgh', &
    'bbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddset', &
    'zzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwop'), &
    MIN('abcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefgh', &
    'bbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddset', &
    'zzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwop'), &
    REPEAT('abab', 64), MAXVAL(charArray), MINVAL(charArray))

    call test1(MAX('abcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefgh', &
    'bbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddset', &
    'zzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwop'), &
    MIN('abcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefgh', &
    'bbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddset', &
    'zzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwop'), &
    REPEAT('abab', 64), MAXVAL(charArray), MINVAL(charArray))

    call test2(MAX('abcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefgh', &
    'bbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddset', &
    'zzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwop'), &
    MIN('abcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefgh', &
    'bbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddset', &
    'zzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwop'), &
    REPEAT('abab', 64), MAXVAL(charArray), MINVAL(charArray))

    call test3(MAX('abcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefgh', &
    'bbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddset', &
    'zzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwop'), &
    MIN('abcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefgh', &
    'bbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddsetbbcddset', &
    'zzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwop'), &
    REPEAT('abab', 64), MAXVAL(charArray), MINVAL(charArray))

    contains

    subroutine test1(tmp, tmp2, tmp3, tmp4, tmp5)

    character(256), VALUE :: tmp, tmp2, tmp3, tmp4, tmp5
    if (tmp /= &
    'zzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwop') &
    error stop 6_4

    if (tmp2 /= &
    'abcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefgh') &
    error stop 7_4

    if (tmp3 /= &
    'abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab') &
    error stop 8_4

    if (tmp4 /= &
    'zzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwop') &
    error stop 9_4

    if (tmp5 /= &
    'abcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefgh') &
    error stop 10_4

        tmp = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        tmp2 = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        tmp3 = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        tmp4 = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        tmp5 = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'

    if (tmp /= &
    'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
    error stop 26_4

    if (tmp2 /= &
    'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
    error stop 27_4

    if (tmp3 /= &
    'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
    error stop 28_4

    if (tmp4 /= &
    'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
    error stop 29_4

    if (tmp5 /= &
    'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
    error stop 30_4    

    end subroutine

    subroutine test2(tmp, tmp2, tmp3, tmp4, tmp5)

        character(255), VALUE :: tmp, tmp2, tmp3, tmp4, tmp5
    if (tmp /= &
    'zzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwo') &
    error stop 11_4

    if (tmp2 /= &
    'abcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefg') &
    error stop 12_4

    if (tmp3 /= &
    'abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababa') &
    error stop 13_4  
    
    if (tmp4 /= &
    'zzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwo') &
    error stop 14_4

    if (tmp5 /= &
    'abcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefg') &
    error stop 15_4

        tmp = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'                                                                                                                        
        tmp2 = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'                                                                                                                       
        tmp3 = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'                                                                                                                       
        tmp4 = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'                                                                                                                       
        tmp5 = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'                                                                                                                       

    if (tmp /= &
    'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
    error stop 31_4

    if (tmp2 /= &
    'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
    error stop 32_4

    if (tmp3 /= &
    'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
    error stop 33_4

    if (tmp4 /= &
    'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
    error stop 34_4

    if (tmp5 /= &
    'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
    error stop 35_4  

    end subroutine

    subroutine test3(tmp, tmp2, tmp3, tmp4, tmp5)

    character(256) :: tmp, tmp2, tmp3, tmp4, tmp5
    if (tmp /= &
    'zzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwop') &
    error stop 16_4

    if (tmp2 /= &
    'abcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefgh') &
    error stop 17_4

    if (tmp3 /= &
    'abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab') &
    error stop 18_4

    if (tmp4 /= &
    'zzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwop') &
    error stop 19_4

    if (tmp5 /= &
    'abcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefgh') &
    error stop 20_4

        tmp = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'                                                                                                                        
        tmp2 = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'                                                                                                                       
        tmp3 = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'                                                                                                                       
        tmp4 = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'                                                                                                                       
        tmp5 = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'                                                                                                                       

    if (tmp /= &
    'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
    error stop 36_4

    if (tmp2 /= &
    'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
    error stop 37_4

    if (tmp3 /= &
    'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
    error stop 38_4

    if (tmp4 /= &
    'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
    error stop 39_4

    if (tmp5 /= &
    'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
    error stop 40_4  

    end subroutine

    subroutine testV(tmp, tmp2, tmp3, tmp4, tmp5)

        character(256) :: tmp, tmp2, tmp3, tmp4, tmp5
	VALUE :: tmp, tmp2, tmp3, tmp4, tmp5
    if (tmp /= &
    'zzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwop') &
    error stop 1_4

    if (tmp2 /= &
    'abcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefgh') &
    error stop 2_4

    if (tmp3 /= &
    'abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab') &
    error stop 3_4

    if (tmp4 /= &
    'zzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwopzzyxqwop') &
    error stop 4_4

    if (tmp5 /= &
    'abcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefghabcdefgh') &
    error stop 5_4

        tmp = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'                                                                                                                        
        tmp2 = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'                                                                                                                       
        tmp3 = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'                                                                                                                       
        tmp4 = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'                                                                                                                       
        tmp5 = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'                                                                                                                       

    if (tmp /= &
    'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
    error stop 21_4

    if (tmp2 /= &
    'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
    error stop 22_4

    if (tmp3 /= &
    'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
    error stop 23_4

    if (tmp4 /= &
    'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
    error stop 24_4

    if (tmp5 /= &
    'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
    error stop 25_4

    end subroutine

   end
