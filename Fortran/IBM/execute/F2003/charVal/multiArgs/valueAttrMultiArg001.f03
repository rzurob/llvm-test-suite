!*  ===================================================================
!*
!*  DATE                       : 04/03/2006
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Passing characters along with multiple
!*				 actual args of different types. character
!*				 actual arg to be tested is 2 bytes.
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


program valueAttrMultiArg001

    character (len=2) :: ch1, ch2
    integer :: in1, in2
    real :: re1

    ch1 = 'bb'
    ch2 = 'hh'
    in1 = 1
    in2 = 2
    re1 = 3.0

    call testV(in2, in1, ch1, ch2, re1)
    if (ch1 /= 'bb') error stop 1_4
    if (ch2 /= 'xx') error stop 2_4

    ch2 = 'hh'

    call test1(ch1, in1, in2, ch2, re1)
    if (ch1 /= 'bb') error stop 3_4
    if (ch2 /= 'xx') error stop 4_4

    ch2 = 'hh'

    call test2(ch1, in1, in2, ch2, re1)
    if (ch1 /= 'bb') error stop 5_4
    if (ch2 /= 'xx') error stop 6_4

    ch2 = 'hh'

    call test3(ch2, in1, in2, re1, ch1)
    if (ch1 == 'bb') error stop 7_4
    if (ch1 /= 'zz') error stop 8_4
    if (ch2 /= 'xx') error stop 9_4

    contains

    subroutine test1(tmp, int1, int2, char2, real1)

       character(2), VALUE :: tmp
       integer :: int1, int2
       character(2) :: char2
       real :: real1

       tmp = 'zz'
       int2 = int2 + 1
       int1 = int1 + int2
       char2 = 'xx'
       real1 = real1 + int2 + int1

    end subroutine

    subroutine test2(tmp, int1, int2, char2, real1)

       character(1), VALUE :: tmp
       integer :: int1, int2
       character(2) :: char2
       real :: real1

       tmp = 'zz'
       int2 = int2 + 1
       int1 = int1 + int2
       char2 = 'xx'
       real1 = real1 + int2 + int1

    end subroutine

    subroutine test3(char2, int1, int2, real1, tmp)

       character(2) :: tmp
       integer :: int1, int2
       character(2) :: char2
       real :: real1

       int2 = int2 + 1
       int1 = int1 + int2
       tmp = 'zz'
       real1 = real1 + int2 + int1
       char2 = 'xx'

    end subroutine

    subroutine testV(int2, int1, tmp, char2, real1)

       character(2) :: tmp
       VALUE :: tmp
       integer :: int1, int2
       character(2) :: char2
       real :: real1

       char2 = 'xx'
       int2 = int2 + 1
       int1 = int1 + int2
       real1 = real1 + int2 + int1
       tmp = 'zz'

    end subroutine

   end
