! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/func_return/ffuncRet007a1.f
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id = -1

        contains

        final :: finalizeBase, finalizeBaseRank1
        procedure :: print => printBase
        procedure, non_overridable :: produceAllocScalar => produceBaseAllocScalar
        procedure, non_overridable :: produceAllocArray => produceBaseAllocArray
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'default'

        contains

        procedure :: print => printChild
    end type

    contains

    function produceBaseAllocScalar (b)
        class (base(4)), allocatable :: produceBaseAllocScalar
        class (base(4)), intent(in) :: b

        allocate (produceBaseAllocScalar, source=b)
    end function

    function produceBaseAllocArray (b, size)
        class (base(4)), allocatable :: produceBaseAllocArray (:)
        class (base(4)), intent(in) :: b
        integer(4), intent(in) :: size

        allocate (produceBaseAllocArray (size), source=b)
    end function

    subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base(4)), intent(in) :: b(:)

        print *, 'finalizeBaseRank1'
    end subroutine

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

module m1
use m
    type (child(4,1,20)), save :: c1_m
    type (base(4)), save :: b1_m
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
