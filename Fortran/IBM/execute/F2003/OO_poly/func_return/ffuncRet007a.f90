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
! %GROUP: ffuncRet007a.f
! %VERIFY: ffuncRet007a.out:ffuncRet007a.vf
! %STDIN:
! %STDOUT: ffuncRet007a.out
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
!*  DATE                       : 06/22/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : poly function return (allocatables will get
!                               deallocated after usage)
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
        integer(4) :: id = -1

        contains

        final :: finalizeBase, finalizeBaseRank1
        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character(20) :: name = 'default'

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base), intent(in) :: b (:)

        print *, 'finalizeBaseRank1'
    end subroutine

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

module m1
use m
    type (child), save :: c1_m(2:3)
    type (base), save :: b1_m (0:2)

    contains

    function makeData (b)
        class (base), allocatable :: makeData (:)
        class (base), intent(in) :: b (:)

        allocate (makeData(lbound(b,1):ubound(b,1)), source=b)
    end function

    integer(4) function dataLength (d)
        class (base), intent(in) :: d (:)

        do i = 1, size (d)
            call d(i)%print
        end do

        dataLength = size (d)
    end function

    subroutine printData1 (d)
        type (base), intent(in) :: d (:)

        do i = 1, size (d)
            call d(i)%print
        end do
    end subroutine
end module

program ffuncRet007a
use m1
    c1_m%id = (/2,3/)
    c1_m%name = (/'c1_m_2', 'c1_m_3'/)

    b1_m%id = (/0,1,2/)

    if (dataLength (makeData (c1_m)) /= 2) error stop 1_4

    call printData1 (makeData (c1_m))

    print *, 'test b1_m'

    if (dataLength (makeData (b1_m)) /= 3) error stop 2_4

    call printData1 (makeData (b1_m))

    print *, 'end'
end
