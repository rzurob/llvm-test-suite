! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr035a2.f
! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/21/2005
!*
!*  DESCRIPTION                : structure constructor (poly-allocatable
!                               components' allocations in structure constructor
!                               using the poly-pointer function return results
!                               as the data source)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (8)
        integer, kind :: k1
        integer(k1)      id

        contains

        procedure :: makeArray2 => makeBaseArray2
    end type

    type, extends(base) :: child(k2,n1)    ! (8,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type

    contains

    class (base(8)) function makeBaseArray2 (b1, shape)
        class (base(8)), intent(in) :: b1
        integer, intent(in) :: shape(2)

        pointer makeBaseArray2 (:,:)

        allocate (makeBaseArray2(shape(1), shape(2)), source=b1)
    end function
end module


module m1
use m
    type dataType(k3)    ! (8)
        integer, kind                :: k3
        class(base(k3)), allocatable :: data(:,:)
    end type
end module


program fconstr035a2
use m1
    call test1 (child(8,1,20)(1, 'test1'))

    contains

    subroutine test1 (b1)
        class (base(8)), intent(in) :: b1

        associate (x => dataType(8) (data = b1%makeArray2 ((/2,2/))))
            if (.not. allocated (x%data)) error stop 1_4

            if (any(shape (x%data) /= (/2,2/))) error stop 2_4

            select type (y => x%data)
                type is (child(8,1,*))
                    if (any (y%id /= 1)) error stop 3_4

                    if (any (y%name /= 'test1')) error stop 4_4

                class default
                    error stop 10_4
            end select
        end associate
    end subroutine
end
