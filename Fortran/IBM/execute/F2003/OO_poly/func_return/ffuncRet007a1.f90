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
! %GROUP: ffuncRet007a1.f
! %VERIFY: ffuncRet007a1.out:ffuncRet007a1.vf
! %STDIN:
! %STDOUT: ffuncRet007a1.out
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
!*  DATE                       : 08/20/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : poly-func-return (poly allocatable function
!                               return as selector in associate construct)
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
        procedure, non_overridable :: produceAllocScalar => produceBaseAllocScalar
        procedure, non_overridable :: produceAllocArray => produceBaseAllocArray
    end type

    type, extends(base) :: child
        character(20) :: name = 'default'

        contains

        procedure :: print => printChild
    end type

    contains

    function produceBaseAllocScalar (b)
        class (base), allocatable :: produceBaseAllocScalar
        class (base), intent(in) :: b

        allocate (produceBaseAllocScalar, source=b)
    end function

    function produceBaseAllocArray (b, size)
        class (base), allocatable :: produceBaseAllocArray (:)
        class (base), intent(in) :: b
        integer(4), intent(in) :: size

        allocate (produceBaseAllocArray (size), source=b)
    end function

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base), intent(in) :: b(:)

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
    type (child), save :: c1_m
    type (base), save :: b1_m
end module

program ffuncRet007a1
use m1
    c1_m%id = 1
    c1_m%name = 'c1_m'

    b1_m%id = 10

    associate (x => c1_m%produceAllocScalar())
        call x%print
    end associate

    associate (x => b1_m%produceAllocArray(2), y => c1_m%produceAllocArray(3))
        do i = lbound(x,1), ubound(x,1)
            call x(i)%print
        end do

        do i = lbound(y,1), ubound(y,1)
            call y(i)%print
        end do
    end associate

    print *, 'end'
end
