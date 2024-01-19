!*  ===================================================================
!*
!*  DATE                       : 04/03/2006
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test 64 byte character variables with
!*                               subroutines attempting to alter the
!*				 variables with and without the value
!*				 attribute.
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


program valueAttrCharArg005

    character (len=64) :: ch

    ch = 'abcdefghijklmnopqrstuvwxyzabcdefabcdefghijklmnopqrstuvwxyzabcdef'

    call testV(ch)
    if (ch /= &
    'abcdefghijklmnopqrstuvwxyzabcdefabcdefghijklmnopqrstuvwxyzabcdef') &
    error stop 1_4

    call test1(ch)
    if (ch /= &
    'abcdefghijklmnopqrstuvwxyzabcdefabcdefghijklmnopqrstuvwxyzabcdef') &
    error stop 2_4

    call test2(ch)
    if (ch /= &
    'abcdefghijklmnopqrstuvwxyzabcdefabcdefghijklmnopqrstuvwxyzabcdef') &
    error stop 3_4

    call test3(ch)
    if (ch == &
    'abcdefghijklmnopqrstuvwxyzabcdefabcdefghijklmnopqrstuvwxyzabcdef') &
    error stop 4_4

    if (ch /= &
    'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') &
    error stop 5_4

    contains

    subroutine test1(tmp)

       character(64), VALUE :: tmp
       tmp = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'

    end subroutine

    subroutine test2(tmp)

       character(63), VALUE :: tmp
       tmp = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'

    end subroutine

    subroutine test3(tmp)

       character(64) :: tmp
       tmp = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'

    end subroutine

    subroutine testV(tmp)

        character(64) :: tmp
	VALUE :: tmp
        tmp = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'

    end subroutine

   end
