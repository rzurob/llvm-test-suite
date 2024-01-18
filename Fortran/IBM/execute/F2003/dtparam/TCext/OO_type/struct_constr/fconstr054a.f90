! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/OO_type/struct_constr/fconstr054a.f
! opt variations: -qnok -qnol -qnodeferredlp

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/23/2005
!*
!*  DESCRIPTION                : structure constructor (overriding the structure
!                               constructor with generic function; use the
!                               unlimited poly allocatable component: rank two)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class(*), allocatable, private :: data(:,:,:)

        contains

        procedure :: shape => dataShape
        procedure :: getData => returnData
    end type

    interface base
        module procedure createBase
    end interface

    contains

    integer function dataShape (b)
        class (base(4,*)), intent(in) :: b
        dimension dataShape(3)

        dataShape = -1

        if (allocated (b%data)) dataShape = shape (b%data)
    end function

    class (*) function returnData (b)
        class (base(4,*)), intent(in) :: b
        allocatable returnData(:)

        allocate (returnData(size(b%data)), source=reshape(b%data, &
                (/size(b%data)/)))
    end function

    !! shape should be something like this '1:3, 2:6, 0:2'
    type (base(4,20)) function createBase (x, shape)
        class(*), intent(in) :: x(:)
        character(*), intent(in) :: shape

        character(30) localBuf
        integer localIndex

        integer lb(3), ub(3)

        localBuf = shape
        localIndex = index(localBuf, ":")

        do while (localIndex /= 0)
            localBuf(localIndex:localIndex) = ' '

            localIndex = index(localBuf, ":")
        end do

        read (localBuf, *) lb(1), ub(1), lb(2), ub(2), lb(3), ub(3)

        allocate (createBase%data(lb(1):ub(1), lb(2):ub(2), lb(3):ub(3)), &
                source=reshape(x, (/ub-lb+1/)))
    end function
end module


program fconstr054a
use m
    class(base(4,:)), pointer :: b1
    class(*), allocatable :: x1(:)

    allocate (b1, source=base((/(j, j=1,8)/), '1:2, 0:1, 2: 3'))

    if (any(b1%shape() /= 2)) error stop 1_4

    allocate (x1(product(b1%shape())), source=b1%getData())

    if (size(x1) /= 8) error stop 2_4

    !! verify the results
    select type (x1)
        type is (integer)
            if (sum(x1) /= 36) error stop 2_4

            if (maxval(x1) /= 8) error stop 3_4
            if (any(maxloc(x1) /= (/8/))) error stop 4_4
        class default
            error stop 5_4
    end select
end
