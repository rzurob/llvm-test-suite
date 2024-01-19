
module m
    type base
        real(8), pointer :: data => null()

        contains

        final :: finalizeArray1, finalizeArray2
    end type

    contains

    subroutine finalizeArray1 (b)
        type (base), intent(inout) :: b(:)

        do i = 1, size(b)
            if (associated (b(i)%data)) then
                write (*, '(a, f10.2)') 'before deallocate, data= ', b(i)%data
                deallocate (b(i)%data)
            end if
        end do
    end subroutine

    subroutine finalizeArray2 (b)
        type (base), intent(inout) :: b(:,:)

        do j = 1, size (b, 2)
            do i = 1, size (b, 1)
                if (associated (b(i,j)%data)) then
                    write (*, '(a, f10.2)') 'before deallocate, data= ', &
                            b(i,j)%data

                    deallocate (b(i,j)%data)
                end if
            end do
        end do
    end subroutine
end module

program ffinal004a2
use m
    class (base), pointer :: b1(:), b2(:,:)

    allocate (b1(3), b2(2,2))

    do i = 0, 2, 2
        allocate (b1(i+1)%data, source=i*1.5_8)
    end do

    do j = 1, 2
        do i = 1, 2
            allocate (b2(i,j)%data, source=10._8*i + j)
        end do
    end do

    deallocate (b1, b2)

    print *, 'end'
end
