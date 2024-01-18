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
! %GROUP: fconstr002.f
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
!*  DESCRIPTION                : structure constructor (default initialization
!*                               for private components)
!*                               pointer types are disassociated; allocatable
!*                               not allocated
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
        integer*4, private :: id = 1
        real*4, pointer, private :: value => null()
    end type

    type, extends(base) :: child
        character(20), private :: name = 'child type'
        real*8, allocatable, private :: extendedValue (:)
    end type

    type (base), save :: b1_m
    type (child), save :: c1_m

    contains

    logical function isBaseCorrect (b, intVal)
        type(base), intent(in) :: b
        integer*4, intent(in) :: intVal

        isBaseCorrect = ( (.not. associated (b%value)) .and. &
                          (b%id .eq. intVal) )
    end function

    logical function isChildCorrect (c, intVal, charVal)
        type (child), intent(in) :: c
        integer*4, intent(in) :: intVal
        character(*), intent(in) :: charVal

        isChildCorrect = ( isBaseCorrect (c%base, intVal) .and. &
                           (.not. allocated (c%extendedValue)) .and. &
                           (c%name .eq. charVal) )
    end function
end module

program fconstr002
use m

    type, extends (base) :: secondChild
        real*4, pointer :: extendedValue (:) => null()
    end type

    type (base) :: b1
    type (child) :: c1
    type (secondChild) :: s1

    if (.not. isBaseCorrect (b1_m, 1_4)) error stop 1_4
    if (.not. isBaseCorrect (b1, 1_4)) error stop 2_4

    if (.not. isChildCorrect (c1_m, 1_4, 'child type')) error stop 3_4
    if (.not. isChildCorrect (c1, 1_4, 'child type')) error stop 4_4

    if (.not. isBaseCorrect (s1%base, 1_4)) error stop 5_4
    if (associated (s1%extendedValue)) error stop 6_4
end
