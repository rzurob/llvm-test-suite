! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/14/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
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

program falloc005a10_1

    type base
        integer(4), allocatable :: shape(:)
    end type

    type (base) :: b1(10)

    class (*), pointer :: x0, x1(:), x2(:,:), x3(:,:,:)

    complex(8) :: cx(5,5,6,4)

    allocate (complex(4) :: x0)

    allocate (real(8) :: x1(-1:10))

    allocate (character(2) :: x2 (0:3, 3:6))

    allocate (integer(8) :: x3 (0:4, 1:3, 10:15))

    !! now we take the shapes of x0-x3, and also cx
    allocate (b1(1)%shape (size(shape(x0))), source=shape(x0))

    allocate (b1(2)%shape (size(shape(x1))), source=shape(x1))

    allocate (b1(3)%shape (size (shape(x2))), source=shape(x2))

    allocate (b1(4)%shape (size(shape(cx))), source=shape(cx))

    allocate (b1(5)%shape (size(shape(x1(-1:4)))), source=shape(x1(-1:4)))

    allocate (b1(7)%shape (size(shape(x2(0:2,4:6)))),  &
            source=shape(x2(0:2,4:6)))


    allocate (b1(8)%shape (size(shape(x3(0:2,2:3,13:15)))), &
            source=shape(x3(0:2,2:3,13:15)))

    allocate (b1(9)%shape (size(shape(cx(1:1, 2:3, 6:5, 3:4)))), &
            source=shape(cx(1:1, 2:3, 6:5, 3:4)))


    allocate (b1(10)%shape (size(shape(x3))), source=shape(x3))

    call printShapes (b1)

    call deallocateShapes (b1)

    contains

    subroutine printShapes (b)
        class (base), intent(in) :: b(:)

        do i = 1, size (b)
            if (allocated (b(i)%shape) .and. (size(b(i)%shape) /= 0)) then
                print *, i, ':', b(i)%shape
            end if
        end do
    end subroutine

    subroutine deallocateShapes (b)
        class (base), intent(inout) :: b(:)

        do i = 1, size (b)
            if (allocated (b(i)%shape)) then
                print *, i
                deallocate (b(i)%shape)
            end if
        end do
    end subroutine
end
