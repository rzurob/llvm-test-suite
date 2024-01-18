!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/14/2005
!*
!*  DESCRIPTION                : class keyword (defined elemental assignment)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real(8), pointer :: data(:) => null()

        contains

        procedure :: getData => getBaseData
        final :: finalizeBase
    end type

    interface assignment(=)
        module procedure copyB1fromB2
    end interface

    interface base
        module procedure makeBaseObj
    end interface

    interface operator (.equal.)
        module procedure compareRealArray
    end interface


    contains

    real(8) function getBaseData (b)
        class(base), intent(in) :: b
        allocatable getBaseData(:)

        if (associated(b%data)) then
            allocate (getBaseData(size(b%data)), source=b%data)
        end if
    end function

    elemental subroutine copyB1fromB2 (b1, b2)
        class(base), intent(out) :: b1
        class (base), intent(in) :: b2

        if (associated(b2%data)) then
            allocate (b1%data(lbound(b2%data,1):ubound(b2%data,1)), &
                        source=b2%data)
        end if
    end subroutine

    elemental subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        if (associated(b%data)) deallocate (b%data)
    end subroutine

    type(base) function makeBaseObj (data)
        real(8), intent(in) :: data(:)

        allocate (makeBaseObj%data(size(data)), source=data)
    end function

    logical function compareRealArray (d1, d2)
        real(8), intent(in) :: d1(:), d2(:)
        dimension compareRealArray (size(d1))

        logical precision_r8

        if (size(d1) /= size(d2)) error stop 10_4

        do i = 1, size(d1)
            compareRealArray(i) = precision_r8(d1(i), d2(i))
        end do
    end function
end module

program fclass016a
use m
    class(base), allocatable :: b1(:)

    allocate (b1(100))

    do i = 1, 50
        allocate (b1(i)%data(i), source=(/(j*1.0_8, j=1,i)/))
    end do

    do i = 51, 100
        b1(i) = base(b1(101-i)%data)
    end do

    !! verify results

    do i = 1, 50
        if (.not. all (b1(i)%getData() .equal. (/(j*1.0_8, j=1,i)/))) &
                    error stop 1_4
    end do

    do i = 51, 100
        if (.not. all ((/(j*1.0_8, j=1,101-i)/) .equal. b1(i)%getData())) &
                    error stop 2_4
    end do

    !! test the array section

    b1(::2) = b1(2::2)

    do i = 1, 50, 2
        if (.not. all (b1(i)%getData() .equal. b1(i+1)%getData())) &
                    error stop 3_4

        if (.not. all (b1(i)%getData() .equal. (/(j*1.0_8, j=1,i+1)/))) &
                    error stop 4_4
    end do

    do i = 51, 100, 2
        if (.not. all (b1(i)%getData() .equal. b1(i+1)%getData())) &
                    error stop 5_4

        if (.not. all (b1(i)%getData() .equal. (/(j*1.0_8, j=1,100-i)/))) &
                    error stop 6_4
    end do

end
