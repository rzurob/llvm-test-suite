! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc005a6.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (array section in source-expr)
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

program falloc005a6
use m
    type (child(4,1,20)) :: c1 (10)
    class (base(4)), allocatable :: b1(:), b2(:), b3(:)

    c1%id = (/(i,i=1,10)/)
    c1%name = (/('c1_'//char(ichar('0')+i-1), i=1, 10)/)

    allocate (b1(size(c1)), source= c1%base)

    if (any (b1%id /= (/1,2,3,4,5,6,7,8,9,10/))) error stop 1_4

    do i = 1, 10, 4
        call b1(i)%print
    end do

    !! second allocate
    allocate (b2(3:5), source=c1(3:7:2))

    call b2(3)%print
    call b2(4)%print
    call b2(5)%print

    !! last allocate
    allocate (b3(-1:-1), source=b2(4:4))

    call b3(-1)%print

    deallocate (b1, b2, b3)
end
