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
! %GROUP: fconstr024.f
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
!*  DATE                       : 12/22/2003
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*                                                                     
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : structure constructor (private parent type)
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
    type, private :: base
        integer*4 :: id
        real*4 :: value
    end type

    type, extends(base) :: child
        character(20) :: name
    end type

    type, extends(child) :: thirdGeneration
        logical*1 :: isSet
    end type

    type (base) :: b1_m = base (1, 1.0)
    type (child) :: c1_m = child (base = base(2, 2.0), name = 'c1_m')

    type (thirdGeneration) :: t1_m = thirdGeneration ( &
                isSet = .true., base = base (3, 3.0), name = 't1_m')

    contains

    logical function isChildCorrect (c, intVal, realVal, charVal)
        type (child), intent(in) :: c
        integer*4, intent(in) :: intVal
        real*4, intent(in) :: realVal
        character(*), intent(in) :: charVal

        isChildCorrect = ((c%id == intVal) .and. (c%value == realVal) &
                    .and. (c%name == charVal))
    end function
end module


program fconstr024
use m

    ! in the main program base type is inaccessible, parent component base is
    ! inaccessible; but id and value are

    type (child) :: c1 = child (2, 2.0, 'c1')

    type (child) :: c2 = child (value = 3.0, name = 'c2', id = 3)

    type (thirdGeneration) :: t1 = thirdGeneration (4, 4, 't1', .true.)

    type (thirdGeneration) :: t2 = thirdGeneration (isSet = .true., &
                    child = child (5, 5.0, 't2'))

    if (.not. isChildCorrect (c1_m, 2, 2.0, 'c1_m')) error stop 1_4

    if (.not. isChildCorrect (t1_m%child, 3, 3.0, 't1_m')) error stop 2_4
    if (.not. t1_m%isSet) error stop 3_4

    if (.not. isChildCorrect (c1, 2, 2.0, 'c1')) error stop 4_4

    if (.not. isChildCorrect (c2, 3, 3.0, 'c2')) error stop 5_4

    if (.not. isChildCorrect (t1%child, 4, 4.0, 't1')) error stop 6_4
    if (.not. t1%isSet) error stop 7_4

    if (.not. isChildCorrect (t2%child, 5, 5.0, 't2')) error stop 8_4
    if (.not. t2%isSet) error stop 9_4
end
