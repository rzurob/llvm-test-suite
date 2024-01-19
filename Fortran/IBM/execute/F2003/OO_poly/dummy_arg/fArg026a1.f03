! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/04/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (dummy-procedure as
!*                               the actual argument; subroutine with implicit
!*                               interface)
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
        integer*4 :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character*20 :: name

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

    subroutine modifyBase (b, sub)
        class (base), intent(inout) :: b (:)

        call sub (b)
    end subroutine
end module

program fArg026a1
use m
    external increaseID4OddIdx

    type (child) :: c1 (10)

    class (base), pointer :: b1 (:)

    allocate (b1(20))

    b1%id = (/(i,i=1,20)/)

    call modifyBase (b1, increaseID4OddIdx)

    call modifyBase (b1(11:20), increaseID4OddIdx)

    do i = 1, 20
        call b1(i)%print
    end do

    c1 = (/(child(i, 'c1_'//char(ichar('0')+i-1)), i=1,10)/)

    call modifyBase (c1(10:1:-1), increaseID4OddIdx)

    do i = 1, 10
        call c1(i)%print
    end do

    deallocate (b1)
end

subroutine increaseID4OddIdx (b)
use m
    type (base), intent(inout) :: b(10)

    b(::2)%id = b(::2)%id + 100
end subroutine
