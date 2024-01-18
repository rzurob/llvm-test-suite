! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr014a.f
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
! %GROUP: fconstr014a.f
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
!*  DATE                       : Nov. 14, 2003
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*                                                                     
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : structure constructor (derived type with
!*                               private component)
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
    type base(k1,k2)    ! (4,4)
        integer, kind     :: k1,k2
        integer(k1)       :: id
        real(k2), private :: value = 10.0
    end type

    type, extends (base) :: child(k3,n1,k4)    ! (4,4,1,20,2)
        integer, kind             :: k3,k4
        integer, len              :: n1
        character(kind=k3,len=n1) :: name
        logical(k4), private      :: isSet = .false.
    end type

    type (child(4,4,1,20,2)), save :: c1_m = child(4,4,1,20,2) (100, 2.0, 'c1_m')
    type (child(4,4,1,20,2)), save :: c2_m = child(4,4,1,20,2) (200, 4.0, 'c2_m', isSet = .true.)

    contains

    logical function isBaseCorrect (b, intVal, realVal)
        type (base(4,4)), intent(in) :: b
        integer*4, intent(in) :: intVal
        real*4, intent(in) :: realVal

        isBaseCorrect = ( (b%id == intVal) .and. (b%value == realVal) )
    end function

    logical function isChildCorrect (c, intVal, realVal, charVal, logVal)
        type (child(4,4,1,*,2)), intent(in) :: c
        integer*4, intent(in) :: intVal
        real*4, intent(in) :: realVal
        character(*), intent(in) :: charVal
        logical*2, intent(in) :: logVal

        isChildCorrect = (isBaseCorrect (c%base, intVal, realVal) .and. &
                    (c%name == charVal) .and. (c%isSet .eqv. logVal) )
    end function
end module

program fconstr014a
use m

    type (child(4,4,1,20,2)) :: c1 = child(4,4,1,20,2) (id = 1, name = 'c1')
    type (child(4,4,1,20,2)) :: c2 = child(4,4,1,20,2) (2, name = 'c2')

    if (.not. isChildCorrect (c1_m, 100, 2.0, 'c1_m', .false._2)) error stop 1_4
    if (.not. isChildCorrect (c2_m, 200, 4.0, 'c2_m', .true._2)) error stop 2_4

    if (.not. isChildCorrect (c1, 1, 10.0, 'c1', .false._2)) error stop 3_4
    if (.not. isChildCorrect (c2, 2, 10.0, 'c2', .false._2)) error stop 4_4
end
