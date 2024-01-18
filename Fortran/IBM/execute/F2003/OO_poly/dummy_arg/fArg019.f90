! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/27/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (nonelemental procedure
!                               referenced by generic-name)
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
        integer*4 :: id = -1
    end type
end module

program fArg019
use m
    interface increaseID
        subroutine addValScalar (b, i)
        use m
            class (base), intent(inout) :: b(:)
            integer*4, intent(in) :: i
        end subroutine

        subroutine addValArray (b, i)
        use m
            class (base), intent(inout) :: b(:)
            integer*4, intent(in) :: i(:)
        end subroutine

        subroutine addScalarByScalar (b, i)
        use m
            class (base), intent(inout) :: b
            integer*4, intent(in) :: i
        end subroutine
    end interface

    class (base), allocatable :: b1(:)

    allocate (b1(3))

    call increaseID (b1, 1)

    if (any(b1%id /= 0)) error stop 1_4

    call increaseID (b1, (/1,2,3/))

    if (any (b1%id /= (/1,2,3/))) error stop 2_4

    call increaseID (b1(3), 10)

    if (any (b1%id /= (/1,2,13/))) error stop 3_4
end


subroutine addValScalar (b, i)
use m
    class (base), intent(inout) :: b(:)
    integer*4, intent(in) :: i

    b%id = b%id + i
end subroutine

subroutine addValArray (b, i)
use m
    class (base), intent(inout) :: b(:)
    integer*4, intent(in) :: i(:)

    b%id = b%id + i
end subroutine


subroutine addScalarByScalar (b, i)
use m
    class (base), intent(inout) :: b
    integer*4, intent(in) :: i

    b%id = b%id + i
end subroutine
