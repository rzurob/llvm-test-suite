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
! %GROUP: fconstr017.f
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
!*  DATE                       : 12/19/2003
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*                                                                     
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : structure constructor (keyword usage in the
!*                               component-spec; ultimate component only)
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
        real*4, private :: value = 1.0
    end type

    type, extends(base) :: child
        character(20) :: name
    end type

    type, extends(child) :: thirdGeneration
        logical*2 :: isSet
    end type

    type (thirdGeneration), save :: t1_m = thirdGeneration (id = 10, &
            name = 't1_m', isSet=.true.)

    type (child), save :: c1_m = child (name = 'c1_m', id = 20)
    type (child), save :: c2_m = child (value = 1.0, name = 'c2_m', id = 25)

    type (base), save :: b1_m = base (value = 0.0, id = 30)

    contains

    logical function isBaseCorrect (b, intVal, realVal)
        type (base), intent(in) :: b
        integer*4, intent(in) :: intVal
        real*4, intent(in) :: realVal

        isBaseCorrect = ((b%id == intVal) .and. (b%value == realVal))
    end function
end module

program fconstr017
use m

    type (thirdGeneration) :: t1 = thirdGeneration (isSet=.true., name = 't1', &
                                                    id = 1)

    type (thirdGeneration) :: t2 = thirdGeneration (name='t2', isSet = .true., &
                                                    id = 2)

    type (thirdGeneration) :: t3 = thirdGeneration ( &
            id = 3, isSet = .true., name ='t3'     )

    type (child) :: c1 = child (id = 4, name = 'c1')
    type (child) :: c2 = child (5, name = 'c2')

    type (base) :: b1 = base (id = 6)


    ! validate all data
    if (.not. isBaseCorrect(b1_m, 30, 0.0)) error stop 1_4

    if (.not. isBaseCorrect(c1_m%base, 20, 1.0)) error stop 2_4
    if (c1_m%name /= 'c1_m') error stop 3_4

    if (.not. isBaseCorrect(c2_m%base, 25, 1.0)) error stop 4_4
    if (c2_m%name /= 'c2_m') error stop 5_4

    if (.not. isBaseCorrect (t1_m%base, 10, 1.0)) error stop 6_4
    if ( (t1_m%name /= 't1_m') .or. (.not. t1_m%isSet)) error stop 7_4

    if (.not. isBaseCorrect (b1, 6, 1.0)) error stop 8_4

    if (.not. isBaseCorrect (c1%base, 4, 1.0)) error stop 9_4
    if (c1%name /= 'c1') error stop 10_4

    if (.not. isBaseCorrect (c2%base, 5, 1.0)) error stop 11_4
    if (c2%name /= 'c2') error stop 12_4

    if (.not. isBaseCorrect (t1%base, 1, 1.0)) error stop 13_4
    if ((t1%name /= 't1') .or. (.not. t1%isSet)) error stop 14_4

    if (.not. isBaseCorrect (t2%base, 2, 1.0)) error stop 15_4
    if ((t2%name /= 't2') .or. (.not. t2%isSet)) error stop 16_4
end
