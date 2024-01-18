! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp /tstdev/F2003/misc/d318784.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/18/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 318784)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,4)
        integer, kind         :: k1
        integer, len          :: n1
        real(k1), allocatable :: d
    end type

    type container(k2,n2)    ! (4,20)
        integer, kind                  :: k2
        integer, len                   :: n2
        class(base(:,k2)), allocatable :: data
    end type

    contains

    recursive subroutine printCo (co)
        !type (container), value :: co
        type (container(4,*)) :: co

        type(container(4,20)) temp

        if (allocated(co%data)) then
            if (allocated(co%data%d)) then
                if (co%data%d > 0) then

                    write (*, '(f10.3)') co%data%d

                    temp = container(4,20)(base(20,4)(co%data%d - 1.0))

                    call printCo (temp)
                else
                    print *, 'finished'
                end if
            else
                print *, 'd is not allocated'
            end if
        end if
    end subroutine
end module

use m
    call printCo (container(4,20)(base(20,4)(3.5)))
end
