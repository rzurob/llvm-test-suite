! GB DTP extension using:
! ftcx_dtp -qck -ql -qdeferredlp -qreuse=self /tstdev/F2003/decimaledit/dtio/dcmlChildRead001.f
! opt variations: -qnock -qnol -qnodeferredlp -qreuse=none

!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 06/29/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test that the decimal edit mode is carried to
!                               child data transfer in read statement by
!                               decimal= in open statement.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,4)
        integer, kind            :: k1
        integer, len             :: n1
        integer(k1)                 id
        complex(k1), allocatable :: cx
        character(:), allocatable :: name

        contains

        procedure :: reset => resetBase
        procedure, pass(dtv) :: readBaseFmtd
        generic :: read(formatted) => readBaseFmtd
    end type

    contains

    subroutine resetBase (b)
        class(base(*,4)), intent(out) :: b
    end subroutine

    subroutine readBaseFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(base(*,4)), intent(inout) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        character(50) fmt

        call dtv%reset

        allocate (dtv%cx)

        if (iotype(1:2) == 'DT') then ! take the format from the supplier
            if (size(v_list) < 1) then

                iostat = 100
                iomsg = 'expecting at least 1 value in v_list'
                return
            end if

            write (fmt, *, decimal='POint') iotype(3:)

            allocate (character(v_list(1)) :: dtv%name)

            read (unit, fmt, iostat=iostat, iomsg=iomsg) dtv%id, dtv%cx, dtv%name
        else if (iotype == 'LISTDIRECTED') then
            read (unit, *, iostat=iostat, iomsg=iomsg) icharLen

            if (iostat /= 0) return

            allocate (character(icharLen) :: dtv%name)

            read (unit, *, iostat=iostat, iomsg=iomsg) dtv%id, dtv%cx, dtv%name
        end if
    end subroutine
end module

program dcmlChildRead001
use m
    class (base(:,4)), allocatable :: b1(:)

    logical(4), external :: precision_x8

    character(:), allocatable :: name

    open (1, file='dcmlChildRead001.in', form='formatted', access='stream', &
            decimal='coMMa', delim='quote')

    write (1, '(100(i5, 1x, 2e15.7, 1x, a7, i3.3))', pos=1, advance='no') &
            (i, cmplx(sin(i*1.0), i*1.0), 'xlftest', i, i=1, 100)


    write (1, *, delim='none') &
        (i, i, cmplx(sin(i*1.0), i*1.0), '"xlftest', i, '"', i = 101, 200)


    !! now read data in b1

    allocate (base(20,4) :: b1(200))

    do i = 1, 200
        b1(i)%id = -i

        allocate (b1(i)%cx, source=cmplx(i))
        allocate (b1(i)%name, source='IBM')
    end do

    read (1, '(100DT"(i5,1x,2e15.7,1x,a)"(10))', pos=1, advance='no') b1(::2)
    read (1, *) b1(2::2)

    do i = 1, 200,2
        if (b1(i)%id /= i/2+1) error stop 1_4

        if (b1(i+1)%id /= 101+i/2) error stop 2_4

        if (.not. precision_x8(b1(i)%cx, cmplx(sin((i/2+1)*1.0), &
                (i/2+1)*1.0,4)))  error stop 3_4

        if (.not. precision_x8(b1(i+1)%cx, cmplx(sin(1.0*(101+i/2)), &
                (101+i/2)*1.0, 4))) error stop 4_4


        allocate (character(10) :: name)

        write (name, '(dc, ss, a,i3.3)') 'xlftest', i/2+1

        if ((len(b1(i)%name) /= 10) .or. (b1(i)%name /= name)) error stop 5_4

        deallocate(name)

        allocate (character(18) :: name)

        write (name, *, decimal='Comma') 'xlftest', 101+i/2

        if ((len(b1(i+1)%name) /= 101+i/2) .or. (b1(i+1)%name /= name(2:))) &
                error stop 6_4

        deallocate(name)
    end do
end
