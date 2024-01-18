! GB DTP extension using:
! ftcx_dtp -qck -qnodefaultpv -qdeferredlp /tstdev/OO_poly/misc/fmisc010b3.f
! opt variations: -qnock -qdefaultpv -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/11/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items (defect 291545)
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
    type base(k1,n1)    ! (1,15)
        integer, kind             :: k1
        integer, len              :: n1
        character(kind=k1,len=n1) :: name = 'default'

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2)    ! (1,15,4)
        integer, kind :: k2
        integer(k2)   :: id = -1

        contains

        procedure :: print => printChild
    end type

    type container(k3)    ! (1)
        integer, kind                  :: k3
        class(base(k3,:)), allocatable :: data(:)

        contains

        procedure :: print => printContainer
    end type

    contains

    subroutine printBase (b)
        class (base(1,*)), intent(in) :: b

        print *, b%name
    end subroutine

    subroutine printChild (b)
        class (child(1,*,4)), intent(in) :: b

        print *, b%name, b%id
    end subroutine

    subroutine printContainer (co)
        class (container(1)), intent(in) :: co

        if (.not. allocated (co%data)) then
            print *, 'data not allocated'
        else
            do i = lbound(co%data,1), ubound(co%data,1)
                call co%data(i)%print
            end do
        end if
    end subroutine
end module

program fmisc010b3
use m
    class (base(1,:)), allocatable :: b1(:,:)

    type (container(1)) :: co1(3)

    allocate (child(1,15,4):: b1(2,3))

    b1%name = reshape ((/'b1_11', 'b1_21', 'b1_12', 'b1_22', 'b1_13', &
                        'b1_23'/), (/2,3/))


    co1 = (/(container(1)(b1(:,j)), j=1,3)/)

    call co1(1)%print
    call co1(2)%print
    call co1(3)%print
end
