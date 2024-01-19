! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/31/2005
!*
!*  DESCRIPTION                : DTIO generics (DTIO on function results; use
!                               rank-two non-poly function results)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer(8), allocatable :: id
    end type

    type, extends(base) :: child
        character(20) :: name
    end type


    interface write(formatted)
        subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
        import base
            class (base), intent(in) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface


    interface makeData
        type (base) function produceBaseArray (id, bShape)
            import base
                integer(8), intent(in) :: id
                class (base), intent(in) :: bShape(:,:)
                dimension produceBaseArray(size(bShape,1),size(bShape,2))
        end function

        type (child) function produceChildArray (id, name, bShape)
        import child
            integer(8), intent(in) :: id
            character(*), intent(in) :: name
            class (child), intent(in) :: bShape(:,:)
            dimension produceChildArray (size(bShape,1),size(bShape,2))
        end function
    end interface

end module

program fdtio512a1
use m
    print *, makeData (10_8, reshape ((/(base(i), i=1,4)/), (/2,2/)))

    print *, makeData (20_8, 'test',  reshape ((/(child(null(), ''), i=1,4)/), &
                    (/2,2/)))
end


subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child
    class (base), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (allocated (dtv%id)) then
        write (unit, *, iostat=iostat, iomsg=iomsg) dtv%id

        if (iostat /= 0) return
    end if

    select type (dtv)
        type is (base)
        type is (child)
            write (unit, *, iostat=iostat, iomsg=iomsg) ', ', dtv%name
        class default
            error stop 10_4
    end select
end subroutine


type (base) function produceBaseArray (id, bShape)
use m, only: base
    integer(8), intent(in) :: id
    class (base), intent(in) :: bShape(:,:)
    dimension produceBaseArray(size(bShape,1),size(bShape,2))

    k = 0

    do i = 1, size (produceBaseArray, 2)
        do j = 1, size (produceBaseArray, 1)
            allocate (produceBaseArray (j, i)%id, source = id + k)
            k = k + 1
        end do
    end do
end function


type (child) function produceChildArray (id, name, bShape)
use m, only: child
    integer(8), intent(in) :: id
    character(*), intent(in) :: name
    class (child), intent(in) :: bShape(:,:)
    dimension produceChildArray (size(bShape,1),size(bShape,2))

    k = 0

    do i = 1, size (produceChildArray, 2)
        do j = 1, size (produceChildArray, 1)
            produceChildArray (j, i) = child(id+k, name)

            k = k + 1
        end do
    end do
end function
