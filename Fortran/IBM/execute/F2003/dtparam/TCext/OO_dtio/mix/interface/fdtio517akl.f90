! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : fdtio517akl
!*
!*  PROGRAMMER                 : David Forster (derived from fdtio517a by Jim Xia)
!*  DATE                       : 2007-08-16 (original: 01/11/2005)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO 
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : DTIO generics (formatted stream read for
!                               pad='no' and advance='no')
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
    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
        real(kbase_1), pointer :: data(:) => null()
    end type


    interface read(formatted)
        subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
        import base
            class (base(4)), intent(inout) :: dtv ! tcx: (4)
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module


subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base
    class (base(4)), intent(inout) :: dtv ! tcx: (4)
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    character openPar, closePar

    if (iotype /= 'LISTDIRECTED') error stop 10_4

    if (size(v_list) /= 0) error stop 11_4

    allocate (dtv%data(2))

    !! child data transfer is always no-advancing IO
    read (unit, '(2(a1,g8.2,a1))', iostat=iostat, iomsg=iomsg) &
            (openPar, dtv%data(i), closePar, i=1,2)
end subroutine


program fdtio517akl
use m
use iso_fortran_env

    class (base(4)), allocatable :: b1(:) ! tcx: (4)

    integer stat1
    character(200) err

    logical precision_r4

    allocate (b1(2))

    open (1, access='stream', form='formatted', pad='no')

    write (1, pos=1, fmt='(3(a1,g8.2,a1), a,a)') '(', 10.2, ')', '(', 3.5, ')', &
                    '(', -1.3, ')', '(1.3)', new_line('a')


    !! EOR should happen during this read for b1(2)
    read (1, *, pos=1, iostat=stat1, iomsg=err) b1

    if (stat1 /= iostat_eor) then
        print *, stat1, err
        error stop 101_4
    end if

    !! verify b1(1) is read in
    if (.not. precision_r4(b1(1)%data(1), 10.0_4)) error stop 2_4

    if (.not. precision_r4(b1(1)%data(2), 3.5_4)) error stop 3_4

    close(1, status='delete')
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 3 changes
