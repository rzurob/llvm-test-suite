! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=none /tstdev/F2003/decimaledit/dtio/dcmlChildRead006.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/17/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test that the decimal edit mode is consistent on
!                               recursive child read; test one an internal file.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type node(n1,k1)    ! (20,4)
        integer, kind             :: k1
        integer, len              :: n1
        real(k1), allocatable     :: data(:)

        type(node(:,k1)), pointer :: next => null()

        contains

        procedure :: readNodeFmtd
        generic :: read(formatted) => readNodeFmtd
        final :: finalizeNode
    end type

    type linkList(k2,n2)    ! (4,20)
        integer, kind             :: k2
        integer, len              :: n2
        type(node(:,k2)), pointer :: head => null()

        contains

        procedure :: readLlistFmtd
        generic :: read(formatted) => readLlistFmtd
    end type

    contains

    recursive subroutine finalizeNode (n)
        type(node(*,4)), intent(inout) :: n

        if (associated(n%next)) deallocate (n%next)
    end subroutine

    recursive subroutine readNodeFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(node(*,4)), intent(inout) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        integer isize

        read (unit, *, iostat=iostat, iomsg=iomsg) isize

        if (iostat /= 0) return

        if (allocated(dtv%data)) deallocate(dtv%data)

        allocate(dtv%data(isize))

        do i = 1, isize
            read (unit, *, iostat=iostat, iomsg=iomsg) dtv%data(i)

            if (iostat /= 0) return
        end do

        allocate (node(20,4) :: dtv%next)

        read (unit, *, iostat=iostat, iomsg=iomsg) dtv%next

        if (iostat /= 0) then
            deallocate (dtv%next)

            iostat = 0
        end if
    end subroutine

    !! assume an empty list in the beginning
    subroutine readLlistFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(linkList(4,*)), intent(inout) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (associated(dtv%head)) deallocate (dtv%head)

        allocate (node(20,4) :: dtv%head)

        read (unit, *, iostat=iostat, iomsg=iomsg) dtv%head

        if (iostat /= 0) then
            deallocate (dtv%head)
            iostat = 0
        end if
    end subroutine
end module

program dcmlChildRead006
use m
    type(linkList(4,20)) list

    character(:), pointer :: string, fmt

    type(node(:,4)), pointer :: iterator

    integer count

    logical(4), external :: precision_r4

    allocate (character(1000) :: string)

    open (1, file='dcmlChildRead006.data', decimal='comma')

    !! test one write data to a string and then read in the list
    write (string, *, decimal='comma') (i, (j*1.1e0, j=1,i), i=1,10)

    read (string, *, decimal='comma') list

    count = 0

    allocate(character(20) ::fmt)

    iterator => list%head

    do while (associated(iterator))
        count = count + 1

        if (.not. allocated(iterator%data)) error stop 1_4

        if (size(iterator%data) /= count) error stop 2_4

        write (fmt, "('(',i5,'(e12.5, 1x))')") count

        write (1, fmt) iterator%data

        iterator => iterator%next
    end do

    if (count /= 10) error stop 3_4

    !! test two: write the data in reverse order and read in the list again
    write(string, *, decimal='comma') (i, (j*1.2, j=i,1,-1), i=10,1,-1)

    read(string, '(dc,DT)') list

    iterator => list%head

    do while (associated(iterator))
        count = count + 1

        if (.not. allocated(iterator%data)) error stop 5_4

        if (size(iterator%data) /= 21-count) error stop 6_4

        do j = 1, 21-count
            if (.not. precision_r4(iterator%data(j), (22-count-j)*1.2_4)) &
                error stop 7_4
        end do

        iterator => iterator%next
    end do

    if (count /= 20) error stop 8_4
end
