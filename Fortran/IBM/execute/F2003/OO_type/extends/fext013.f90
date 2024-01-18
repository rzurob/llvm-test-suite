!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fext013.f
! %VERIFY: 
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*                                                                     
!*  TEST CASE TITLE            :
!*                                                                     
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : Nov. 07, 2003
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*                                                                     
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : derived-type extension (name conflict is
!*                               allowed for unaccessible parent's component;
!*                               three generations are used)
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
module m
    type base
        integer*4 :: id
        real*4, private :: value
    end type

    contains

    subroutine setValue (b, realVal)
        type (base), intent(inout) :: b
        real*4, intent(in) :: realVal

        b%value = realVal
    end subroutine

    real*4 function getValue (b)
        type (base), intent(in) :: b

        getValue = b%value
    end function
end module

module m1
use m
    type, extends(base) :: child
        character(20) :: name
    end type

    type, extends(child) :: thirdGeneration
        integer(8) :: value   ! name conflict is allowed here
    end type

    type (thirdGeneration) :: t1_m
end module

program fext013
    use m1
    type (thirdGeneration) :: t1

    t1%id = 20
    t1%name = 'This is a test'
    t1%value = 100
    call setValue(t1%base, -10.0)

    t1_m%child%id = 10
    t1_m%child%name = 't1_m'
    call setValue (t1_m%child%base, 1.0)
    t1_m%value = 200

    if (t1%base%id /= 20) error stop 1_4
    if (t1%name /= 'This is a test') error stop 2_4
    if (t1%value /= 100) error stop 3_4
    if (getValue(t1%base) /= -10.0) error stop 4_4

    if (t1_m%id /= 10) error stop 5_4
    if (t1_m%name /= 't1_m') error stop 6_4
    if (t1_m%value /= 200) error stop 7_4
    if (getValue(t1_m%child%base) /= 1.0) error stop 8_4
end
