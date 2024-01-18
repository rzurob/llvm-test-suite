!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : valueAttrCharArg008.f
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
!*  DESCRIPTION                : Test 0 byte character variables with 
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


program valueAttrCharArg008

    character (len=0) :: zero
    character (len=0), parameter :: zeroBase = ''

    zero = ''

    call testV(zero)
    if (zero /= zeroBase) error stop 1_4

    call test1(zero)
    if (zero /= zeroBase) error stop 2_4

    call test2(zero)
    if (zero /= zeroBase) error stop 3_4

    call test3(zero)
    if (zero /= zeroBase) error stop 4_4

    call test4(zero)
    if (zero /= zeroBase) error stop 5_4

    contains

    subroutine test1(tmp)

        character(0), VALUE :: tmp
        tmp = ''

    end subroutine

    subroutine test2(tmp)

        character(0), VALUE :: tmp
        tmp = 'z'

    end subroutine

    subroutine test3(tmp)

        character(0) :: tmp
        tmp = ''

    end subroutine

    subroutine test4(tmp)

        character(0) :: tmp
        tmp = 'z'

    end subroutine

    subroutine testV(tmp)

        character(0) :: tmp
	VALUE :: tmp
        tmp = ''

    end subroutine

   end
