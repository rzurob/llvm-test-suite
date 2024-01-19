!*  ===================================================================
!*
!*  DATE                       : 04/03/2006
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test 32 byte character variables with
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


program valueAttrCharArg004

    character (len=32) :: ch

    ch = 'abcdefghijklmnopqrstuvwxyzabcdef'

    call testV(ch)
    if (ch /= 'abcdefghijklmnopqrstuvwxyzabcdef') error stop 2_4

    call test1(ch)
    if (ch /= 'abcdefghijklmnopqrstuvwxyzabcdef') error stop 2_4

    call test2(ch)
    if (ch /= 'abcdefghijklmnopqrstuvwxyzabcdef') error stop 3_4

    call test3(ch)
    if (ch == 'abcdefghijklmnopqrstuvwxyzabcdef') error stop 4_4
    if (ch /= 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') error stop 5_4

    contains

    subroutine test1(tmp)

        character(32), VALUE :: tmp
        tmp = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'

    end subroutine

    subroutine test2(tmp)

        character(31), VALUE :: tmp
        tmp = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'

    end subroutine

    subroutine test3(tmp)

        character(32) :: tmp
        tmp = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'

    end subroutine

    subroutine testV(tmp)

        character(32) :: tmp
	VALUE :: tmp
        tmp = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'

    end subroutine

   end
