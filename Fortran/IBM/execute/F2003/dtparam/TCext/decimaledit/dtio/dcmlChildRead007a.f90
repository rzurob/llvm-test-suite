! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=self /tstdev/F2003/decimaledit/dtio/dcmlChildRead007a.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/18/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test that the decimal edit mode set in a child's
!                               write statement on an internal file has no
!                               effects on other child read statements.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type dataType(k1)    ! (4)
        integer, kind         :: k1
        real(k1), allocatable :: data(:)

        contains

        procedure :: readDataTypeFmtd
        generic :: read(formatted) => readDataTypeFmtd
    end type

    type base(k2)    ! (4)
        integer, kind                    :: k2
        class(dataType(k2)), allocatable :: data
        type(dataType(k2)), pointer      :: data2 => null()

        contains

        procedure :: readBaseFmtd
        generic :: read(formatted) => readBaseFmtd
    end type

    character(:), allocatable :: string

    contains

    subroutine resetString
        if (allocated (string)) deallocate (string)

        allocate(character(8000) :: string)

        string(:) =''
    end subroutine


    integer function stringIndex()
        stringIndex = len (trim(string)) + 1
    end function

    subroutine readDataTypeFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(dataType(4)), intent(inout) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        integer arraySize

        read (unit, *, iostat=iostat, iomsg=iomsg) arraySize

        if (iostat /= 0) return

        if (allocated(dtv%data)) deallocate (dtv%data)

        allocate (dtv%data(arraySize))

        do i = 1, arraySize
            read (unit, *, iostat=iostat, iomsg=iomsg) dtv%data(i)

            if (iostat /= 0) return
        end do
    end subroutine


    !! dtv%data is reverse in decimal edit mode
    subroutine readBaseFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(base(4)), intent(inout) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        character(5) mode

        inquire (unit, decimal=mode, iostat=iostat, iomsg=iomsg)

        if (iostat /= 0) return

        if (mode == 'POINT') then
            mode = 'COMMA'
        else
            mode = 'POINT'
        end if

        if (allocated(dtv%data)) deallocate (dtv%data)

        allocate (dtv%data)

        read (unit, '(DT)', iostat=iostat, iomsg=iomsg) dtv%data

        if (iostat /= 0) return

        !! write out the data to the string
        write (string(stringIndex():), '(20e12.4)', decimal=mode, &
            iostat=iostat, iomsg=iomsg) dtv%data%data

        if (iostat /= 0) return


        if (associated(dtv%data2)) deallocate (dtv%data2)

        allocate (dtv%data2)

        read (unit, *, iostat=iostat, iomsg=iomsg) dtv%data2

        if (iostat /= 0) return

        write (string(stringIndex():), '(20e12.4)', decimal=mode, &
            iostat=iostat, iomsg=iomsg) dtv%data2%data
    end subroutine
end module

program dcmlChildRead007a
use m
    class(base(4)), allocatable :: b1(:)

    character(:), allocatable :: fmt, verifyBuffer

    logical(4), external :: precision_r4

    allocate (b1(20))

    open (1, file='dcmlChildRead007.data')


    allocate(character(50) :: fmt)

    do i = 1, 10
        write (fmt, '("(dc, i5,1x,", i5, "e15.8, i5,1x,", i5, "e15.8)")') i, i

        write (1, fmt) i, (j*1.1, j=1,i), i, (j*1.1, j=1,i)
    end do

    do i = 11, 20
        write (fmt, '("(i5,1x,", i5, "e15.8, i5,1x,", i5, "e15.8)")') i, i

        write (1, fmt) i, (j*1.1, j=1,i), i, (j*1.1, j=1,i)
    end do

    rewind 1

    call resetString

    read (1, '(dc, 10DT)') b1(1:10)

    read (1, *) b1(11:20)

    !! verify b1
    do i = 1, 20
        if ((.not. allocated(b1(i)%data)) .or. (.not. associated(b1(i)%data2)))&
            error stop 1_4


        do j = 1, i
            if (.not. precision_r4(b1(i)%data%data(j), j*1.1_4)) error stop 2_4

            if (.not. precision_r4(b1(i)%data2%data(j), j*1.1_4)) error stop 3_4
        end do
    end do

    !! verify string
    allocate(character(6000) :: verifyBuffer)

    do i = 1, 10
        write (verifyBuffer(i*(i-1)*12+1:), '(20e12.4)') &
            (j*1.1, j=1,i), (j*1.1, j=1,i)
    end do

    do i = 11, 20
        write (verifyBuffer(i*(i-1)*12+1:), '(40(dc, e12.4))') &
            (j*1.1, j=1,i), (j*1.1, j=1,i)
    end do

    if (verifyBuffer /= string) error stop 4_4
end
