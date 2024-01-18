! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/30/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO generics (parent data transfer uses
!                               internal file)
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
    type base
        integer, pointer :: data(:)
    end type

    integer ioUnit

    interface read(formatted)
        subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
        import base
            class (base), intent(inout) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module


program fdtio055
use m
    character(1000) dataStream
    character(200) err
    integer stat
    type (base) :: b1, b2(0:1)
    integer, target, allocatable :: i1(:)
    integer, pointer :: i2(:)

    nullify (b1%data)
    allocate(i1(1),i2(10000))

    b2(0)%data => i1
    b2(1)%data => i2

    dataStream = '3, 3, 2 1'

    read (dataStream, *, iostat=stat, iomsg=err) b1

    if (stat /= 0) then
        print *, stat, err
        error stop 1_4
    end if

    if (ioUnit >= 0) error stop 10_4

    !! verify b1%data
    if (any(b1%data /= (/3,2,1/))) error stop 2_4

    dataStream = trim(dataStream) // ', 2, -1,0'

    read (dataStream, *, iostat=stat, iomsg=err) b2

    if (ioUnit >= 0) error stop 11_4

    if (stat /= 0) then
        print *, stat, err
        error stop 3_4
    end if

    !! verify b2%data
    if (any(b2(0)%data /= (/3,2,1/))) error stop 4_4

    if (any (b2(1)%data /= (/-1, 0/))) error stop 5_4

end


subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only:base, ioUnit
    class (base), intent(inout) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    integer dataSize, stat1

    ioUnit = unit
    if (associated (dtv%data)) then
        deallocate (dtv%data, stat=stat1)
    end if

    read (unit, *, iostat=iostat, iomsg=iomsg) dataSize

    if (iostat /= 0) return

    if (dataSize <= 0) return

    allocate (dtv%data(dataSize))

    read (unit, *, iostat=iostat, iomsg=iomsg) dtv%data
end subroutine

