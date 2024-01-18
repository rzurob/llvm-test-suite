! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/allocEnh/argAssociation/dummyArg010.f
! opt variations: -qnol -qnodeferredlp -qreuse=self

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/24/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test case for allocating data while reading a
!                               input data file.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1,k2)    ! (20,4,4)
        integer, kind         :: k1,k2
        integer, len          :: n1
        integer(k1), pointer  :: id
        real(k2), allocatable :: data

        contains

        procedure :: readBaseFmtd
        generic :: read(formatted) => readBaseFmtd
    end type

    contains

    subroutine readBaseFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(base(*,4,4)), intent(inout) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        allocate (dtv%id)

        dtv%data = 0

        read (unit, *, iostat=iostat, iomsg=iomsg) dtv%id, dtv%data
    end subroutine

    subroutine readData (unit, b1)
        integer, intent(in) :: unit
        type(base(:,4,4)), allocatable, intent(out) :: b1(:)

        integer stat
        type(base(20,4,4)) localTemp

        b1 = [base(20,4,4) :: ]

        stat = 0

        read (unit, *, iostat=stat) localTemp

        do while (stat == 0)
            b1 = [b1, localTemp]

            read (unit, *, iostat=stat) localTemp
        end do
    end subroutine
end module

program dummyArg010
use m
    type (base(:,4,4)), allocatable :: b1(:)

    integer unit

    logical(4), external :: precision_r4

    unit = 10000

    open (unit, file='dummyArg010.data')

    write (unit, *) (i, log(i*1.5), new_line('a'), i=1, 10000)

    rewind unit

    call readData (unit, b1)

    if (.not. allocated(b1)) error stop 1_4

    if (size(b1) /= 10000) error stop 2_4

    do i = 1, size(b1)
        if ((.not. associated(b1(i)%id)) .or. &
            (.not. allocated(b1(i)%data))) error stop 3_4

        if (b1(i)%id /= i) error stop 4_4

        if (.not. precision_r4(b1(i)%data, log(i*1.5))) error stop 5_4
    end do
end
