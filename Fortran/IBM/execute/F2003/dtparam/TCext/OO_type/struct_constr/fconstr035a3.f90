! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr035a3.f
!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 02/21/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : structure constructor (the poly allocatable
!                               components' allocations in structure
!                               constructor for function result as the data
!                               source)
!*
!*
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

    contains

    type (base(8)) function makeBaseArray2 (b1, shape)
        class (base(8)), intent(in) :: b1
        integer, intent(in) :: shape(2)

        dimension makeBaseArray2 (2:shape(1)+1, 2:shape(2)+1)

        makeBaseArray2%id = b1%id
    end function
end module


module m1
use m
    type dataType(k3)    ! (8)
        integer, kind                :: k3
        class(base(k3)), allocatable :: data(:,:)
    end type
end module


program fconstr035a3
use m1
    call test1 (child(8,1,20)(1, 'test1'))

    contains

    subroutine test1 (b1)
        class (base(8)), intent(in) :: b1

        call associate ( dataType(8) (data = makeBaseArray2 (b1, (/3,3/))))

    end subroutine

!    associate (x => dataType(8) (data = makeBaseArray2 (b1, (/3,3/))))
    subroutine associate (x)
        type(dataType(8)), intent(in) :: x
        if (.not. allocated (x%data)) error stop 1_4

        if (any(shape (x%data) /= (/3,3/))) error stop 2_4

        if (any (lbound(x%data) /= 1)) error stop 3_4
        if (any (ubound(x%data) /= 3)) error stop 4_4

        select type (y => x%data)
            type is (base(8))
                if (any (y%id /= 1)) error stop 5_4

            class default
                error stop 10_4
        end select
    end subroutine
end
