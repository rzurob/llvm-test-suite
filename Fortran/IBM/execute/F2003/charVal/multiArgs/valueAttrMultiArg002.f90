!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : valueAttrMultiArg002.f
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
!*				 actual arg to be tested is 8 bytes.
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


program valueAttrMultiArg002

    character (len=8) :: eight, eight2
    integer :: in1, in2
    integer(8) :: in3, in4
    logical :: log1, log2
    real :: re1, re2, re3

    eight = 'abcdefgh'
    eight2 = 'abcdefgh'
    in1 = 1
    in2 = 3
    in3 = 900001
    in4 = 41455321
    log1 = .true.
    log2 = .true.
    re1 = 1.23456
    re2 = 9.876543
    re3 = 5.1928374655

    call testV(in1, in2, in3, in4, log1, log2, re1, re2, re3, eight2, eight)
    if (eight /= 'abcdefgh') error stop 1_4
    if (eight2 /= 'yyyyyyyy') error stop 1_4

    eight2 = 'abcdefgh'

    call test1(eight, in1, in2, in3, in4, log1, log2, re1, re2, re3, eight2)
    if (eight /= 'abcdefgh') error stop 1_4
    if (eight2 /= 'yyyyyyyy') error stop 1_4

    eight2 = 'abcdefgh'

    call test2(in1, eight, in2, in3, in4, log1, log2, re1, re2, re3, eight2)
    if (eight /= 'abcdefgh') error stop 2_4
    if (eight2 /= 'yyyyyyyy') error stop 1_4

    eight2 = 'abcdefgh'

    call test3(in1, in2, in3, in4, log1, log2, eight, re1, re2, re3, eight2)
    if (eight == 'abcdefgh') error stop 3_4
    if (eight /= 'xxxxxxxx') error stop 4_4
    if (eight2 /= 'yyyyyyyy') error stop 1_4

    contains

    subroutine test1(tmp, int1, int2, int3, int4, logic1, logic2, real1, real2, real3, char1)

        character(8), VALUE :: tmp
	character(8) :: char1
        integer, VALUE :: int1, int2
        integer(8) :: int3, int4
        logical :: logic1, logic2
        real, VALUE :: real1, real2, real3
	char1 = 'yyyyyyyy'
	if (logic1) then
          int1 = (int3 - int2) * int4
	  real1 = (int1 + int2) / int3
	  real3 = real1 + real2
          tmp = 'xxxxxxxx'
	else if (logic2) then
	  int2 = int1
	  int4 = int3
	  logic1 = logic2
          real3 = real1 + real2 + real3
          tmp = 'xxxxxxxx'
	end if

    end subroutine

    subroutine test2(int1, tmp, int2, int3, int4, logic1, logic2, real1, real2, real3, char1)

        character(7), VALUE :: tmp
	character(8) :: char1
        integer, VALUE :: int1, int2
        integer(8) :: int3, int4
        logical :: logic1, logic2
        real, VALUE :: real1, real2, real3
	char1 = 'yyyyyyyy'
	if (logic1) then
          int1 = (int3 - int2) * int4
	  real1 = (int1 + int2) / int3
	  real3 = real1 + real2
	else if (logic2) then
	  int2 = int1
	  int4 = int3
	  logic1 = logic2
          real3 = real1 + real2 + real3
	end if
        tmp = 'xxxxxxxx'

    end subroutine

    subroutine test3(int1, int2, int3, int4, logic1, logic2, tmp, real1, real2, real3, char1)

        character(8) :: tmp
	character(8) :: char1
        integer, VALUE :: int1, int2
        integer(8) :: int3, int4
        logical :: logic1, logic2
        real, VALUE :: real1, real2, real3
	char1 = 'yyyyyyyy'
        tmp = 'xxxxxxxx'
	if (logic1) then
          int1 = (int3 - int2) * int4
	  real1 = (int1 + int2) / int3
	  real3 = real1 + real2
	else if (logic2) then
	  int2 = int1
	  int4 = int3
	  logic1 = logic2
          real3 = real1 + real2 + real3
	end if

    end subroutine

    subroutine testV(int1, int2, int3, int4, logic1, logic2, real1, real2, real3, char1, tmp)

        character(8) :: tmp
	VALUE :: tmp
	character(8) :: char1
        integer, VALUE :: int1, int2
        integer(8) :: int3, int4
        logical :: logic1, logic2
        real, VALUE :: real1, real2, real3
	char1 = 'yyyyyyyy'
	if (logic1) then
          int1 = (int3 - int2) * int4
	  real1 = (int1 + int2) / int3
	  real3 = real1 + real2
          tmp = 'xxxxxxxx'
	else if (logic2) then
	  int2 = int1
	  int4 = int3
	  logic1 = logic2
          real3 = real1 + real2 + real3
          tmp = 'xxxxxxxx'
	end if

    end subroutine

   end
