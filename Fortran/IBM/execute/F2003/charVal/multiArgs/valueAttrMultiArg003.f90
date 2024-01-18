!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : valueAttrMultiArg003.f
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
!*  DESCRIPTION                : Passing characters along with multiple
!*				 actual args of different types. character
!*				 actual arg to be tested is 16 bytes.
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


program valueAttrMultiArg003

    character (len=16) :: ch
    character (len=2) :: ch2
    character (len=8) :: ch3

    integer :: in1, in2, in3
    logical :: logic1
    real :: re1, re2    

    ch = 'abcdefghijklmnop'
    ch2 = 'xy'
    ch3 = 'rfvedcas'
    in1 = 543
    in2 = 321
    in3 = 768
    re1 = 744.22456
    re2 = 0.100001001
    logic1 = .true.

    call testV(ch, ch2, ch3, in1, in2, in3, logic1, re1, re2)
    if (ch /= 'abcdefghijklmnop') error stop 1_4

    call test1(ch, ch2, ch3, in1, in2, in3, logic1, re1, re2)
    if (ch /= 'abcdefghijklmnop') error stop 2_4

    call test2(ch, ch2, ch3, in1, in2, in3, logic1, re1, re2)
    if (ch /= 'abcdefghijklmnop') error stop 3_4

    call test3(ch, ch2, ch3, in1, in2, in3, logic1, re1, re2)
    if (ch == 'abcdefghijklmnop') error stop 4_4
    if (ch /= 'xxxxxxxxxxxxxxxx') error stop 5_4

    contains

    subroutine test1(tmp, tmp2, tmp3, int1, int2, int3, log1, real1, real2)

        character(16), VALUE :: tmp
	character(2) :: tmp2
	character(8) :: tmp3
        integer :: int1, int2, int3
        logical :: log1
        real :: real1, real2 

        if (log1) then
	    real1 = real2 + (int1*1.0)
	    tmp = 'xxxxxxxxxxxxxxxx'
	else
	    tmp = MAX(tmp2, tmp3)
	end if
	    tmp = 'xxxxxxxxxxxxxxxx'

    end subroutine

    subroutine test2(tmp, tmp2, tmp3, int1, int2, int3, log1, real1, real2)

        character(15), VALUE :: tmp
	character(2) :: tmp2
	character(8) :: tmp3
        integer :: int1, int2, int3
        logical :: log1
        real :: real1, real2 

        if (log1) then
	    real1 = real2 + (int1*1.0)
	    tmp = 'xxxxxxxxxxxxxxx'
	else
	    tmp = MAX(tmp2, tmp3)
	end if
	    tmp = 'xxxxxxxxxxxxxxx'

    end subroutine

    subroutine test3(tmp, tmp2, tmp3, int1, int2, int3, log1, real1, real2)

        character(16) :: tmp
	character(2) :: tmp2
	character(8) :: tmp3
        integer :: int1, int2, int3
        logical :: log1
        real :: real1, real2 

        if (log1) then
	    real1 = real2 + (int1*1.0)
	    tmp = 'xxxxxxxxxxxxxxxx'
	else
	    tmp = MAX(tmp2, tmp3)
	end if
	    tmp = 'xxxxxxxxxxxxxxxx'

    end subroutine

    subroutine testV(tmp, tmp2, tmp3, int1, int2, int3, log1, real1, real2)

        character(16) :: tmp
	VALUE :: tmp
	character(2) :: tmp2
	character(8) :: tmp3
        integer :: int1, int2, int3
        logical :: log1
        real :: real1, real2 

        if (log1) then
	    real1 = real2 + (int1*1.0)
	    tmp = 'xxxxxxxxxxxxxxxx'
	else
	    tmp = MAX(tmp2, tmp3)
	end if
	    tmp = 'xxxxxxxxxxxxxxxx'

    end subroutine

   end
