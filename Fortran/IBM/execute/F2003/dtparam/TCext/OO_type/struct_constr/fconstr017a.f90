! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr017a.f
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
! %GROUP: fconstr017a.f
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id
    end type

    type, extends(base) :: child(k2,n1,k3)    ! (4,1,20,4)
        integer, kind             :: k2,k3
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
        real(k3), private         :: value = 1.0
    end type

    type, extends(child) :: thirdGeneration(k4)    ! (4,1,20,4,2)
        integer, kind :: k4
        logical(k4)   :: isSet
    end type

    type (thirdGeneration(4,1,20,4,2)), save :: t1_m = thirdGeneration(4,1,20,4,2) (id = 10, &
            name = 't1_m', isSet=.true.)

    type (child(4,1,20,4)), save :: c1_m = child(4,1,20,4) (name = 'c1_m', id = 20)

    contains

    logical function isChildCorrect (c, intVal, realVal, charVal)
        type (child(4,1,*,4)), intent(in) :: c
        integer*4, intent(in) :: intVal
        real*4, intent(in) :: realVal
        character(*), intent(in) :: charVal

        isChildCorrect = ((c%id == intVal) .and. (c%value == realVal) &
                          .and. (c%name == charVal))
    end function

    subroutine addValue (c, realVal)
        type (child(4,1,*,4)), intent(inout) :: c
        real*4, intent(in) :: realVal

        c%value = c%value + realVal
    end subroutine
end module

program fconstr017a
use m

    type (thirdGeneration(4,1,20,4,2)) :: t1 = thirdGeneration(4,1,20,4,2) (isSet=.true., name = 't1', &
                                                    id = 1)

    type (thirdGeneration(4,1,20,4,2)) :: t2 = thirdGeneration(4,1,20,4,2) (name='t2', isSet = .true., &
                                                    id = 2)

    type (thirdGeneration(4,1,20,4,2)) :: t3 = thirdGeneration(4,1,20,4,2) ( &
            id = 3, isSet = .true., name ='t3'     )


    type (child(4,1,20,4)) :: c1 = child(4,1,20,4) (4, name = 'c1')
    type (child(4,1,20,4)) :: c2 = child(4,1,20,4) (name = 'c2', id = 5)

    ! validate all data
    if (.not. isChildCorrect (t1%child, 1, 1.0, 't1')) error stop 1_4
    if (.not. t1%isSet) error stop 2_4

    if (.not. isChildCorrect (t2%child, 2, 1.0, 't2')) error stop 3_4
    if (.not. t2%isSet) error stop 4_4

    call addValue (t3%child, 2.0)

    if (.not. isChildCorrect (t3%child, 3, 3.0, 't3')) error stop 5_4
    if (.not. t3%isSet) error stop 6_4

    if (.not. isChildCorrect (c1_m, 20, 1.0, 'c1_m')) error stop 7_4

    if (.not. isChildCorrect (t1_m%child, 10, 1.0, 't1_m')) error stop 8_4
    if (.not. t1_m%isSet) error stop 9_4

    if (.not. isChildCorrect (c1, 4, 1.0, 'c1')) error stop 10_4
    if (.not. isChildCorrect (c2, 5, 1.0, 'c2')) error stop 11_4

    call addValue (c2, 1.0)

    if (.not. isChildCorrect (c2, 5, (1.0+1.0), 'c2')) error stop 12_4
end
