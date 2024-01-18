! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc005a4.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (variables with poly-allocatable
!                               components in source-expr)
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

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'default'

        contains

        procedure :: print => printChild
    end type

    contains

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
    type container(k3)    ! (4)
        integer, kind                :: k3
        class(base(k3)), allocatable :: data (:)
    end type
end module

program falloc005a4
use m1
    class (container(4)), allocatable :: b1
    type (container(4)), pointer :: b2

    allocate (b2)
    allocate (b2%data (0:1), source=(/child(4,1,20)(1,'temp1'),child(4,1,20)(2,'temp2')/))

    allocate (b1, source = b2)

    if ((lbound (b1%data,1) /= 0) .or. (ubound(b2%data,1) /= 1)) error stop 1_4

    call b1%data(0)%print
    call b1%data(1)%print
end
