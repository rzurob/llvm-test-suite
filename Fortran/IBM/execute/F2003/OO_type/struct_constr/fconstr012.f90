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
! %GROUP: fconstr012.f
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
!*  DATE                       : Nov. 12, 2003
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*                                                                     
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : structure constructor (parent component
!*                               initialization via an object)
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
        integer*4 ::id
        real*4 :: value
    end type

    type, extends(base) :: child
        character*20 :: name
    end type

    type, extends(child) :: thirdGeneration
        logical*1 :: isSet
    end type

    type (base) :: b1_m = base (10, 1.0)
    type (child) :: c1_m
    type (thirdGeneration) :: t1_m

    contains

    logical function isChildCorrect (c, intVal, realVal, charVal)
        type (child), intent(in) :: c
        integer*4, intent(in) :: intVal
        real*4, intent(in) :: realVal
        character(*), intent(in) :: charVal

        isChildCorrect = ((c%id == intVal) .and. (c%value == realVal) &
                    .and. (c%name == charVal))
    end function

    subroutine initializeModuleData
        c1_m = child (base = b1_m, name = 'c1_m')

        t1_m = thirdGeneration (child = child (name = 't1_m', base = b1_m), &
                            isSet = .true.)

    end subroutine
end module

program fconstr012
use m

    type (base) :: b1 = base (1, 1.0)
    type (child) :: c1, c2, c3
    type (thirdGeneration) :: t1, t2

    c1 = child(base = b1, name = 'test data c1')
    c2 = child (base = c1%base, name = c1%name)

    c3 = child (b1%id, b1%value, c1%name)

    t1 = thirdGeneration (child = c1, isSet = .true.)
    t2 = thirdGeneration (base = c1%base, name ='t2', isSet = .true.)

    if (.not. isChildCorrect (c1, 1, 1.0, 'test data c1')) error stop 1_4

    if (.not. isChildCorrect (c2, 1, 1.0, 'test data c1')) error stop 2_4

    if (.not. isChildCorrect (c3, 1, 1.0, 'test data c1')) error stop 3_4

    if (.not. isChildCorrect (t1%child, 1, 1.0, 'test data c1')) error stop 4_4
    if (.not. t1%isSet) error stop 5_4

    if (.not. isChildCorrect (t2%child, 1, 1.0, 't2')) error stop 6_4
    if (.not. t2%isSet) error stop 7_4

    call initializeModuleData

    if (.not. isChildCorrect (c1_m, 10, 1.0, 'c1_m')) error stop 8_4

    if (.not. isChildCorrect (t1_m%child, 10, 1.0, 't1_m')) error stop 9_4
    if (.not. t1_m%isSet) error stop 10_4
end
