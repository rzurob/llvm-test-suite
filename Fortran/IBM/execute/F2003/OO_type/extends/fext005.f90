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
! %GROUP: fext005.f
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
!*  DESCRIPTION                : derived-type extended (component inherited,
!                                parent introduced via use, private parent components)
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
        integer, private :: id = 10
    end type

    contains
        logical function isBaseCorrect (b, intVal)
            type (base), intent (in) :: b
            integer, intent (in) :: intVal

            isBaseCorrect = (b%id .eq. intVal)
        end function
end module

module m1
use m
    type, extends(base) :: child
        character(20) :: name
    end type

    type (base), save :: b1_m
    type (child), save :: c1_m

    contains
        logical function isChildCorrect (c, intVal, charVal)
            type(child), intent (in) :: c
            integer, intent (in) :: intVal
            character(*), intent (in) :: charVal

            isChildCorrect = (isBaseCorrect (c%base, intVal) .and. (c%name .eq. charVal))
        end function
end module

program fext005
    use m1
    type (child) :: c1

    c1%name = 'This is a test'
    c1_m%name = 'c1_m'

    if (.not. isChildCorrect(c1, 10, 'This is a test')) error stop 1_4

    if (.not. isBaseCorrect (b1_m, 10)) error stop 2_4

    if (.not. isChildCorrect(c1_m, 10, 'c1_m')) error stop 3_4
end
