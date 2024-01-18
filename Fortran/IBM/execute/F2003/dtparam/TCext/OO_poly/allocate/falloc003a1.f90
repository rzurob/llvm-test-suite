! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc003a1.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/06/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (source=; test the reshape used as the
!                               source-expr for rank-two array)
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

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2,n1,k3)    ! (4,1,20,4)
        integer, kind             :: k2,k3
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'default'
        type(base(k3))            :: data (3)

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*,4)), intent(in) :: b

        print *, b%id, b%name

        do i = 1, 3
            call b%data(i)%print
        end do
    end subroutine
end module

program falloc003a1
use m
    class (base(4)), allocatable :: b1 (:,:)

    type (child(4,1,20,4)) :: c1(4)

    allocate (b1(2,2), source=reshape(c1, (/2,2/)))

    call b1(1,1)%print
    call b1(1,2)%print
    call b1(2,1)%print
    call b1(2,2)%print
end
