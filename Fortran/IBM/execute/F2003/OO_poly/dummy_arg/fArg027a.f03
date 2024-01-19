! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/04/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (assumed-shape array used
!                               as the actual argument to an assumed-size array)
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

    private internalT

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine interalT (b)
        type (base), intent(inout) :: b(4)

        b(2:4:2)%id = b(2:4:2)%id *2
    end subroutine

    subroutine test1 (b)
        class (base), intent(inout) :: b (:,:)

        call interalT (b)
    end subroutine

end module

program fArg027a
use m
    class (base), allocatable :: b1(:,:)


    allocate (b1(2,2), source=reshape ((/child(1,'b1_1'), child(2,'b1_2'), &
                    child(3, 'b1_3'), child(4,'b1_4')/), (/2,2/)))



    call test1 (b1)

    call b1(1,1)%print
    call b1(2,1)%print
    call b1(1,2)%print
    call b1(2,2)%print
end

