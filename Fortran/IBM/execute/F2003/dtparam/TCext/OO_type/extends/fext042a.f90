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
! %GROUP: fext042a.f
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
!*  DATE                       : 02/11/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*                                                                     
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : extends (test of accessibility of components;
!*                               inherited components retain their
!*                               accessibilities)
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
    type, private :: base(k1)
        integer, kind :: k1
        integer(k1) :: id
    end type

    type, extends(base) :: child(n)
        integer, len :: n=20
        private     !! this private does NOT affect parent's component id
        character(n) :: name = ''

        contains

        procedure :: assgnName => assgnName2Child
    end type

    type (base(4)) :: b1_m
    type (child(4)), save :: c1_m

    private assgnName2Child

    contains

    subroutine initializeC1_m
        c1_m%id = 10
        c1_m%name = 'c1_m'
    end subroutine

    subroutine assgnName2Child (c, s)
        class (child(4,*)), intent(inout) :: c
        character(*), intent(in) :: s

        c%name = s
    end subroutine

    logical function isChildValCorrect (c, i, s)
        type (child(4,20)), intent(in) :: c
        integer*4, intent(in) :: i
        character(*), intent(in) :: s

        isChildValCorrect = ((c%id == i) .and. (c%name == s))
    end function
end module

program fext042a
use m
    type (child(4,20)) :: c1

    c1 = child (4)(id = 1)

    call c1%assgnName ('c1')

    if (.not. isChildValCorrect (c1, 1, 'c1')) error stop 1_4

    c1%id = 2

    if (.not. isChildValCorrect (c1, 2, 'c1')) error stop 2_4

    call initializeC1_m

    if (.not. isChildValCorrect (c1_m, 10, 'c1_m')) error stop 3_4

    b1_m%id = 20

    if (b1_m%id /= 2*c1_m%id) error stop 4_4
end
