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
    type base
        integer(4) :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character(20) :: name

        contains

        procedure :: print => printChild
    end type

    contains

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
    type dataType
        class (base), allocatable :: data1 (:)
        class (base), pointer :: data2(:) => null()
    end type
end module

program falloc005a7
use m1
    type (dataType) :: d1
    type (child) :: c1 (10)

    class (base), allocatable :: b1(:)
    class (base), pointer :: b2(:)

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
