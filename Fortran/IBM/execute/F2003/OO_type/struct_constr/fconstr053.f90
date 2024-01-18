!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/21/2005
!*
!*  DESCRIPTION                : structure constructor (generic name override
!                               the structure constructor; use the rank-one
!                               unlimited poly pointer array as the component)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type container
        private
        class(*), pointer :: data(:,:) => null()

        contains

        procedure :: print => printContainer
        procedure :: clear => freeData
    end type

    interface container
        module procedure makeContainerData1
        module procedure makeContainerData2
    end interface

    private makeContainerData1, makeContainerData2
    contains

    subroutine printContainer (co)
        class (container), intent(in) :: co

        if (associated (co%data)) then
            do j = lbound(co%data, 2), ubound(co%data, 2)
                do i = lbound(co%data,1), ubound(co%data,1)
                    call printItem (co%data(i, j))
                end do
            end do
        end if
    end subroutine

    subroutine printItem (x)
        class(*), intent(in) :: x

        select type (x)
            type is (integer)
                print *, x
            type is (character(*))
                print *, x
            type is (real)
                write (*, '(f12.3)') x
            class default
                print *, 'not a predefined type'
        end select
    end subroutine

    subroutine freeData (co)
        class (container), intent(inout) :: co

        if (associated(co%data)) deallocate(co%data)
    end subroutine

    type (container) function makeContainerData1 (x1, ishape)
        class(*), intent(in) :: x1(:)
        integer, intent(in) :: ishape (2)

        allocate (makeContainerData1%data(ishape(1), ishape(2)), &
                    source=reshape(x1, ishape))
    end function

    type (container) function makeContainerData2 (x2)
        class (*), intent(in) :: x2(:,:)

        allocate (makeContainerData2%data (size(x2,1), size(x2,2)), source=x2)
    end function
end module

program fconstr053
use m
    type (container) :: co1

    class(*), allocatable :: r1(:,:)
    class (*), pointer :: l1(:)

    !! first test
    co1 = container ((/1,2,3,4/), (/2,2/))

    call co1%print

    !! second test
    associate (x => container (reshape ((/'ibm', 'xlf', 'com'/), (/3,1/))))
        call x%print
    end associate

    !! third test
    call co1%clear

    allocate (r1(2,3), source=reshape((/(j*1.5, j=1,12,2)/), (/2,3/)))

    co1 = container (r1)

    call co1%print

    !! last test
    allocate (logical :: l1(2))

    associate (y => container (l1, (/1,2/)))
        call y%print
    end associate
end
