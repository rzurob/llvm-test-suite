! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=none /tstdev/F2003/misc/d318785.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/18/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 318785)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      id

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b)
        type(base(*,4)), intent(inout) :: b

        print *, 'reset id from', b%id, 'to -1'
        b%id = -1
    end subroutine
end module

module m1
use m
    type container(k2,n2)    ! (4,20)
        integer, kind                 :: k2
        integer, len                  :: n2
        type(base(:,k2)), allocatable :: data
    end type

    contains

    subroutine printCo (co)
        type(container(4,20)), value :: co

        if (allocated(co%data)) then
            print *, co%data%id

            deallocate(co%data)
        end if
        print *, 'done'
    end subroutine
end module

use m1
    call printCo (container(4,20)(base(20,4)(100)))
end
