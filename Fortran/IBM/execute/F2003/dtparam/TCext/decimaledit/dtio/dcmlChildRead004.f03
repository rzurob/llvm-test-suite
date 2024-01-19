! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/decimaledit/dtio/dcmlChildRead004.f
! opt variations: -ql -qreuse=self

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/17/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test that .T./.F. are valid format for logical
!                               during child list-directed read in both comma
!                               mode and point mode; test both external and
!                               internal files.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,k2,k3)    ! (4,4,4)
        integer, kind            :: k1,k2,k3
        integer(k1)                 id
        complex(k2), allocatable :: cx(:)
        logical(k3)                 flag

        contains

        procedure :: readBaseFmtd
        generic :: read(formatted) => readBaseFmtd
    end type

    contains

    subroutine readBaseFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(base(4,4,4)), intent(inout) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        integer cxSize

        read (unit, *, iostat=iostat, iomsg=iomsg) dtv%id, cxSize

        if (iostat /= 0) return

        if (allocated(dtv%cx)) deallocate (dtv%cx)

        allocate (dtv%cx(cxSize))

        do i = 1, cxSize
            read (unit, *, iostat=iostat, iomsg=iomsg) dtv%cx(i)

            if (iostat /= 0) return
        end do

        read (unit, *, iostat=iostat, iomsg=iomsg) dtv%flag
    end subroutine
end module

program dcmlChildRead004
use m
    class (base(4,4,4)), allocatable :: b1(:)

    character(:), pointer :: c, string

    character(*), parameter :: flag(0:1) =  (/'.T.', '.F.'/)

    logical(4), external :: precision_x8


    allocate (character(100) :: c)

    allocate (character(2000) :: string)


    string(:) = ''

    open (1, file='dcmlChildRead004.data', decimal='comma')

    do i = 1, 10
        write (c, *) '("2*",sp, i3.2, 1x," ; ",', i, &
            '(" (", d25.16, " ; ", d25.16, ")"), " ; ', flag(mod(i,2)), '", ss)'

        write (1, c) i, (k*1.1, k*2.2, k=1,i)
    end do

    write (string, *, decimal='commA') (j, ' ; ', j, ' ; ', &
            (cmplx(k*1.2, k*3.1), k=1,j), ' ; .T.  \t ', j=1,10)

    rewind 1

    allocate (b1(20))

    open (1, decimal='Point')

    read (1, '(dc, 10dt)') b1(::2)

    read (string, '(10dt)', decimal='comma') b1(2::2)

    !! verify the read in data
    do i = 1, 10
        if (b1(2*i-1)%id /= i) error stop 1_4

        if (b1(2*i)%id /= i) error stop 2_4

        if (size(b1(2*i-1)%cx) /= i) error stop 3_4

        if (size(b1(2*i)%cx) /= i) error stop 4_4

        do j = 1, i
            if (.not. precision_x8(b1(2*i-1)%cx(j), cmplx(j*1.1, j*2.2,4))) &
                    error stop 5_4

            if (.not. precision_x8(b1(2*i)%cx(j), cmplx(j*1.2, j*3.1,4))) &
                    error stop 6_4
        end do

        if (b1(2*i-1)%flag .neqv. mod(i,2) == 0) error stop 7_4

        if (.not. b1(2*i)%flag) error stop 8_4
    end do
end
