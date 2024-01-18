! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/func_return/ffuncRet007a.f
! SCCS ID Information
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
!*
!*  DATE                       : 06/22/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id = -1

        contains

        final :: finalizeBase, finalizeBaseRank1
        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'default'

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base(4)), intent(in) :: b (:)

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
    type (child(4,1,20)), save :: c1_m(2:3)
    type (base(4)), save :: b1_m (0:2)

    contains

    function makeData (b)
        class (base(4)), allocatable :: makeData (:)
        class (base(4)), intent(in) :: b (:)

        allocate (makeData(lbound(b,1):ubound(b,1)), source=b)
    end function

    integer(4) function dataLength (d)
        class (base(4)), intent(in) :: d (:)

        do i = 1, size (d)
            call d(i)%print
        end do

        dataLength = size (d)
    end function

    subroutine printData1 (d)
        type (base(4)), intent(in) :: d (:)

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
