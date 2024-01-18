! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc005a7.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/13/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (structure component as the
!                               source-expr)
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
        integer(k1)   :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

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
    type dataType(k3)    ! (4)
        integer, kind                :: k3
        class(base(k3)), allocatable :: data1 (:)
        class(base(k3)), pointer     :: data2(:) => null()
    end type
end module

program falloc005a7
use m1
    type (dataType(4)) :: d1
    type (child(4,1,20)) :: c1 (10)

    class (base(4)), allocatable :: b1(:)
    class (base(4)), pointer :: b2(:)

    c1%id = (/(i,i=1,10)/)
    c1%name = (/('c1_'//char(ichar('0')+i), i=0,9)/)

    allocate (d1%data1(3:5), source=c1(3:5))

    allocate (d1%data2(7:10), source=c1(7:10))

    allocate (b1(3), source=d1%data1)
    allocate (b2(2:5), source=d1%data2)

    call b1(1)%print
    call b1(2)%print
    call b1(3)%print

    call b2(2)%print
    call b2(3)%print
    call b2(4)%print
    call b2(5)%print
end
