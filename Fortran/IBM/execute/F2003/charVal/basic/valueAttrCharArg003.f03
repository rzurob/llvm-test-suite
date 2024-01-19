!*  ===================================================================
!*
!*  DATE                       : 04/03/2006
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test 16 byte character variables with
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


program valueAttrCharArg003

    character (len=16) :: ch

    ch = 'abcdefghijklmnop'

    call testV(ch)
    if (ch /= 'abcdefghijklmnop') error stop 1_4

    call test1(ch)
    if (ch /= 'abcdefghijklmnop') error stop 2_4

    call test2(ch)
    if (ch /= 'abcdefghijklmnop') error stop 3_4

    call test3(ch)
    if (ch == 'abcdefghijklmnop') error stop 4_4
    if (ch /= 'xxxxxxxxxxxxxxxx') error stop 5_4

    contains

    subroutine test1(tmp)

        character(16), VALUE :: tmp
        tmp = 'xxxxxxxx'

    end subroutine

    subroutine test2(tmp)

        character(15), VALUE :: tmp
        tmp = 'xxxxxxxxxxxxxxx'

    end subroutine

    subroutine test3(tmp)

        character(16) :: tmp
        tmp = 'xxxxxxxxxxxxxxxx'

    end subroutine

    subroutine testV(tmp)

        character(16) :: tmp
	VALUE :: tmp
        tmp = 'xxxxxxxxxxxxxxxx'

    end subroutine

   end
