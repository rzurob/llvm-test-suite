!*  ===================================================================
!*
!*  DATE                       : 07/20/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE with DTP
!*                              (type-spec used in ALLOCATE statement;
!                               uses derived types)
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

module amod
    type base
        contains
        procedure :: print => printBase
    end type

    type, extends (base) :: child(k, l)
        integer, kind :: k = 4
        integer, len  :: l = 10
        integer(k) :: id = -1


        contains
        procedure :: print => printChild
    end type

    type, extends(child) :: gen3
        character(l) :: name = 'default'

        contains
        procedure :: print => printGen3
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, 'empty type'
    end subroutine

    subroutine printChild (b)
        class (child(4, *)), intent(in) :: b

        print *, 'child'
        print *, b%id
        print *, "string len = ", b%l
    end subroutine

    subroutine printGen3 (b)
        class (gen3(4, *)), intent(in) :: b

        print *, 'gen3 type'
        print *, b%id, b%name
        print *, "string len = ", b%l
    end subroutine
end module

program dtParamAlloc03
use amod
    class (base), pointer :: b1
    class (base), allocatable :: b2 (:)
    integer len

    allocate (gen3(4, 20) :: b1, b2(2:3))

    call b1%print

    call b2(2)%print
    call b2(3)%print

    deallocate (b1, b2)

    len = 30
    allocate (child(4, len) :: b2(500:501), b1)

    call b1%print

    call b2(500)%print
    call b2(501)%print

    deallocate (b1, b2)
end
