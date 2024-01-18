! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all /tstdev/OO_poly/dummy_arg/fArg019a.f
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id = -1
    end type

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'default'
    end type
end module

program fArg019a
use m
    interface increaseID
        subroutine addValScalar (b, i)
        use m
            class (base(4)), intent(inout) :: b(:)
            integer*4, intent(in) :: i
        end subroutine

        subroutine addValArray (b, i)
        use m
            class (base(4)), intent(inout) :: b(:)
            integer*4, intent(in) :: i(:)
        end subroutine

        subroutine addScalarByScalar (b, i)
        use m
            class (base(4)), intent(inout) :: b
            integer*4, intent(in) :: i
        end subroutine
    end interface

    class (base(4)), allocatable :: b1(:)

    class (child(4,1,20)), pointer :: c1 (:)
    integer*4 vec (2)

    allocate (b1(10))
    allocate (c1(5))


    vec = (/1, 3/)

    call increaseID (b1(1:4), 1)

    call increaseID (b1(5:7), (/1,2,3/))

    call increaseID (b1(8), 10)

    call increaseID (b1(::8), 2)

    call increaseID (c1%base, 1)

    call increaseID (c1(::2)%base, 2)

    call increaseID (c1(2::2)%base, c1(vec)%base%id)


    if (any (b1%id /= (/2,0,0,0,0,1,2,9,1,-1/))) error stop 1_4

    if (any (c1%id /= 2)) error stop 2_4

    deallocate (c1)
    deallocate (b1)
end


subroutine addValScalar (b, i)
use m
    class (base(4)), intent(inout) :: b(:)
    integer*4, intent(in) :: i

    b%id = b%id + i
end subroutine

subroutine addValArray (b, i)
use m
    class (base(4)), intent(inout) :: b(:)
    integer*4, intent(in) :: i(:)

    b%id = b%id + i
end subroutine


subroutine addScalarByScalar (b, i)
use m
    class (base(4)), intent(inout) :: b
    integer*4, intent(in) :: i

    b%id = b%id + i
end subroutine
