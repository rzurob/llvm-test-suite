!*  ===================================================================
!*
!*  DATE                       : 04/03/2006
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test 2 byte character variables with
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


program valueAttrCharArg001

    character (len=2) :: two

    two = 'bb'

    call testV(two)
    if (two /= 'bb') error stop 1_4

    call test1(two)
    if (two /= 'bb') error stop 2_4

    call test2(two)
    if (two /= 'bb') error stop 2_4

    call test3(two)
    if (two == 'bb') error stop 3_4
    if (two /= 'zz') error stop 4_4

    contains

    subroutine test1(tmp)

        character(2), VALUE :: tmp
        tmp = 'zz'

    end subroutine

    subroutine test2(tmp)

        character(1), VALUE :: tmp
        tmp = 'zz'

    end subroutine

    subroutine test3(tmp)

        character(2) :: tmp
        tmp = 'zz'

    end subroutine

    subroutine testV(tmp)

        character(2) :: tmp
	VALUE :: tmp
        tmp = 'zz'

    end subroutine

   end
