!*  ===================================================================
!*
!*  DATE                       : 04/03/2006
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test 8 byte character variables with
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


program valueAttrCharArg002

    character (len=8) :: eight

    eight = 'abcdefgh'

    call testV(eight)
    if (eight /= 'abcdefgh') error stop 1_4

    call test1(eight)
    if (eight /= 'abcdefgh') error stop 1_4

    call test2(eight)
    if (eight /= 'abcdefgh') error stop 2_4

    call test3(eight)
    if (eight == 'abcdefgh') error stop 3_4
    if (eight /= 'xxxxxxxx') error stop 4_4

    contains

    subroutine test1(tmp)

        character(8), VALUE :: tmp
        tmp = 'xxxxxxxx'

    end subroutine

    subroutine test2(tmp)

        character(7), VALUE :: tmp
        tmp = 'xxxxxxx'

    end subroutine

    subroutine test3(tmp)

        character(8) :: tmp
        tmp = 'xxxxxxxx'

    end subroutine

    subroutine testV(tmp)

        character(8) :: tmp
	VALUE :: tmp
        tmp = 'xxxxxxxx'

    end subroutine

   end
