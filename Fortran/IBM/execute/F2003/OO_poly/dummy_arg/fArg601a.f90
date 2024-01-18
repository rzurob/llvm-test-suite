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
! %GROUP: fArg601a.f
! %VERIFY: fArg601a.out:fArg601a.vf
! %STDIN:
! %STDOUT: fArg601a.out
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
!*  DATE                       : 12/03/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (unlimited poly
!                               explicit-shape array dummy-arg; sequence
!                               association)
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
        integer(4) :: id
    end type

    type, extends(base) :: child
        character (15) :: name
    end type

    contains

    function iType (x)
        class (*), intent(in) :: x (2)

        select type (x)
            type is (base)
                print *, x
                iType = 1
            type is (child)
                print *, x
                iType = 2
            type is (integer(4))
                print *, x
                iType = 3
            class default
                print *, 'other type'
                iType = -1
        end select
    end function
end module

program fArg601a
use m

    type (child), target :: c1(5)
    class (base), pointer :: b2(:)

    integer(4) :: i(10), retVal

    i = 10

    c1%id = (/1,2,3,4,5/)
    c1%name = (/'c1_1', 'c1_2', 'c1_3', 'c1_4', 'c1_5'/)

    allocate (b2(0:3), source=(/(child(j, 'xlftest 101'), j=0,3)/))

    retVal = iType (i)

    if (retVal /= 3) error stop 1_4

    retVal = iType (b2)

    if (retVal /= 2) error stop 2_4

    retVal = iType (b2(1:3:2))

    if (retVal /= 2) error stop 3_4

    retVal = iType (c1)

    if (retVal /= 2) error stop 4_4

    deallocate (b2)

    b2 => c1(2::3)

    retVal = iType (b2)

    if (retVal /= 2) error stop 5_4
end
