! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/19/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (implicit interface for
!                               poly-actual-arg and with assumed-size dummy-arg)
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
end module

program fArg026a
use m
    type (base) :: b1 (10)
    class (base), allocatable :: b2 (:)
    type (child), target :: c1 (10)

    class (base), pointer :: b3(:)

    b1 = (/(base (i), i=10,1,-1)/)
    c1 = (/(child(i, 'c1'), i=1,10)/)

    allocate (b2 (5), source=c1(::2))

    call print3 (b1)

    !! this prints out only 1 3 5
    call print3 (b2)
end

subroutine print3 (b)
    use m
    type (base), intent(in) :: b(*)

    call b(1)%print
    call b(2)%print
    call b(3)%print
end subroutine