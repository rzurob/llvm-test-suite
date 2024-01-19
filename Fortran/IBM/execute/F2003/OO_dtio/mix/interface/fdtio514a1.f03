! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/10/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO generics (end of file encountered while
!                               reading internal file)
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
        integer(8), allocatable :: id

        contains

        procedure :: read => readBase
    end type

    type, extends(base) :: child
        character(20), allocatable :: name

        contains

        procedure :: read => readChild
    end type

    character(*), parameter :: separator = '|'

    contains

    subroutine readBase (b, unit, iotype, vlist, iostat, iomsg)
        class (base), intent(inout) :: b
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: vlist(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        !! we only deal with list-directed

        if (iotype /= 'LISTDIRECTED') then
            error stop 10_4
        end if

        if (size(vlist) /= 0) error stop 11_4


        if (.not. allocated (b%id)) allocate (b%id)

        read (unit, *, iostat=iostat, iomsg=iomsg) b%id
    end subroutine


    subroutine readChild (b, unit, iotype, vlist, iostat, iomsg)
        class (child), intent(inout) :: b
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: vlist(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (iotype /= 'LISTDIRECTED') then
            error stop 20_4
        end if

        if (size(vlist) /= 0) error stop 21_4

        if (.not. allocated (b%id)) allocate (b%id)

        if (.not. allocated (b%name)) allocate (b%name)

        read (unit, *, iostat=iostat, iomsg=iomsg) b%id, b%name
    end subroutine
end module


module n
    interface read (formatted)
        subroutine readFormatted(dtv, unit, iotype, vlist, iostat, iomsg)
        use m, only: base
            class (base), intent(inout) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: vlist(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module


subroutine readFormatted(dtv, unit, iotype, vlist, iostat, iomsg)
use m, only: base
    class (base), intent(inout) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    call dtv%read (unit, iotype, vlist, iostat, iomsg)
end subroutine

program fdtio514a1
use m
use n
use iso_fortran_env
    class (base), allocatable :: b1(:), b2(:)

    character(20) data1
    character(10) data2

    integer stat1
    character(200) err

    data1 = '12345, abcdefghxyz1'

    allocate (child:: b1(2))

    !! this read will fail due to EOF
    read (data1, *, iostat=stat1, iomsg=err) b1

   if (stat1 /= iostat_end) then
       print *, stat1, err
       error stop 1_4
   end if

    allocate (b2(10))


    data2 = '10, 11, 12'

    !! the read stmt will fail due to EOF condition
    read (data2, *, iostat=stat1, iomsg=err) b2(1:6)

    if (stat1 /= iostat_end) then
        print *, stat1, err
        error stop 2_4
    end if
end
