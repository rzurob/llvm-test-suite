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
! %GROUP: fconstr028a.f
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
!*  DATE                       : 12/23/2003
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*                                                                     
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : structure constructor for derived type with
!*                               type-bound proc
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
        contains

        procedure, nopass :: print => basePrint
    end type

    type, extends(base) :: child
        character*20 :: name = ''
    end type

    type, extends(child) :: thirdGeneration
        logical*1 :: isSet
    end type

    contains

    subroutine basePrint
        print *, 'base'
    end subroutine
end module

program fconstr028a
use m

    type (child) :: c1 = child ('c1')
    type (child) :: c2 = child (name = 'c2')

    type (thirdGeneration) :: t1 = thirdGeneration(isSet = .true.)
    type (thirdGeneration) :: t2 = thirdGeneration (name = 't2', isSet=.true.)
    type (thirdGeneration) :: t3 = thirdGeneration ('t3', .true.)

    call t3%print()


    ! validate all variables

    if (c1%name /= 'c1') error stop 1_4

    if (c2%name /= 'c2') error stop 2_4

    if ((t1%name /= '') .or. (t1%name /= t1%child%name)) error stop 3_4

    if (.not. t1%isSet) error stop 4_4

    if ((t2%name /= 't2') .or. (t2%name /= t2%child%name)) error stop 5_4

    if (.not. t2%isSet) error stop 6_4

    if ((t3%name /= 't3') .or. (t3%name /= t3%child%name)) error stop 7_4

    if (.not. t3%isSet) error stop 8_4

end
