! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr032a7.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/22/2005
!*
!*  DESCRIPTION                : structure constructor (allocatable components'
!                               allocations in structure constructor; data
!                               source is array sections and pointer arrays)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (8)
        integer, kind :: k1
        integer(k1)      id
    end type

    type, extends(base) :: child(k2,n1)    ! (8,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type

    type container(k3,k4,n2)    ! (8,1,20)
        integer, kind                       :: k3,k4
        integer, len                        :: n2
        class(child(k3,k4,n2)), allocatable :: data(:)
    end type
end module

program fconstr032a7
use m
    type (child(8,1,20)), target :: c1(10)
    class (child(8,1,20)), pointer :: c2(:)

    c1 = (/(child(8,1,20)(i, name='test'), i=1, 10)/)

    c2 => c1(::3)

    call associate1 ( container(8,1,20) (c1(2::2)))

    call associate2 (container(8,1,20) (c2))

    contains

!    associate (x => container(8,1,20) (c1(2::2)))
    subroutine associate1 (x)
        type(container(8,1,*)), intent(in) :: x
        if (.not. allocated (x%data)) error stop 1_4

        if ((lbound(x%data,1) /= 1) .or. (ubound(x%data, 1) /= 5)) error stop 2_4

        if (any (x%data%id /= (/2,4,6,8,10/))) error stop 3_4
        if (any (x%data%name /= 'test')) error stop 4_4
    end subroutine

!    associate (x => container(8,1,20) (c2))
    subroutine associate2 (x)
        type(container(8,1,*)), intent(in) :: x
        if (.not. allocated (x%data)) error stop 5_4

        if ((lbound(x%data,1) /= 1) .or. (ubound(x%data,1) /= 4)) error stop 6_4

        if (any (x%data%id /= (/1, 4, 7, 10/))) error stop 7_4
        if (any (x%data%name /= 'test')) error stop 8_4
    end subroutine
end