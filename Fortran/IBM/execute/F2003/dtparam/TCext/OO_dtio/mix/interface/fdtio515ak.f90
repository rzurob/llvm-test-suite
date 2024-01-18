! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : fdtio515ak
!*
!*  PROGRAMMER                 : David Forster (derived from fdtio515a by Jim Xia)
!*  DATE                       : 2007-08-16 (original: 01/06/2005)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO 
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : DTIO generics (effects of null values in the
!                               input record)
!                               adaptation: exposed kind
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
    type base1 (kbase1_1) ! kbase1_1=8
       integer, kind :: kbase1_1
        integer(kbase1_1), allocatable :: data
    end type

    type base2 (kbase2_1) ! kbase2_1=8
        real(kbase2_1), allocatable :: data
    end type


    interface read(formatted)
        subroutine formattedRead1 (dtv, unit, iotype, v_list, iostat, iomsg)
        import base1
            class (base1(8)), intent(inout) :: dtv ! tcx: (8)
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine

        subroutine formattedRead2 (dtv, unit, iotype, v_list, iostat, iomsg)
        import base2
            class (base2(8)), intent(inout) :: dtv ! tcx: (8)
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module


subroutine formattedRead1 (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base1
    class (base1(8)), intent(inout) :: dtv ! tcx: (8)
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (allocated (dtv%data)) deallocate (dtv%data)

    allocate (dtv%data, source=-1_8)

    read (unit, *, iostat=iostat, iomsg=iomsg) dtv%data
end subroutine


subroutine formattedRead2 (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base2
    class (base2(8)), intent(inout) :: dtv ! tcx: (8)
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (allocated (dtv%data)) deallocate (dtv%data)

    allocate (dtv%data, source=-1.e0_8)

    read (unit, *, iostat=iostat, iomsg=iomsg) dtv%data
end subroutine


program fdtio515ak
use m
    class (base1(8)), allocatable :: b1(:) ! tcx: (8)
    class (base2(8)), allocatable :: b2 (:) ! tcx: (8)

    logical precision_r8

    integer stat1
    character(200) err

    allocate (b1(2), b2(2))

    open (3, file='fdtio515ak.data')

    write (3, *) '10,, 1*,10.35, 1.24'   !<-- this has 5 records

    rewind (3)

    read (3, *, iostat=stat1, iomsg=err) b1, b2

    !! verify data

    if (stat1 /= 0) then
        print *, stat1, err
        error stop 101_4
    end if

    if ((.not. allocated (b1(1)%data)) .or. (.not. allocated (b1(2)%data))) &
                error stop 2_4

    if ((.not. allocated (b2(1)%data)) .or. (.not. allocated (b2(2)%data))) &
                error stop 3_4

    if ((b1(1)%data /= 10) .or. (b1(2)%data /= -1)) error stop 4_4


    if ((.not. precision_r8 (b2(1)%data, -1.e0_8)) .or. (.not. precision_r8 &
        (b2(2)%data, 10.35e0_8))) &
                error stop 5_4


    close (3, status = 'delete')
end


! Extensions to introduce derived type parameters:
! type: base1 - added parameters (kbase1_1) to invoke with (8) / declare with (8) - 3 changes
! type: base2 - added parameters (kbase2_1) to invoke with (8) / declare with (8) - 3 changes
