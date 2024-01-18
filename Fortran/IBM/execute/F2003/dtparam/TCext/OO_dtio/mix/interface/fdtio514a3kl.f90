! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : fdtio514a3kl
!*
!*  PROGRAMMER                 : David Forster (derived from fdtio514a3 by Jim Xia)
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
!*  DESCRIPTION                : DTIO generics (unformatted stream read will
!                               result in EOF if read beyond the end of file)
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
    type base (kbase_1) ! kbase_1=8
       integer, kind :: kbase_1
        complex(kbase_1), allocatable :: data(:)
    end type

    interface read(unformatted)
        subroutine unformattedRead (dtv, unit, iostat, iomsg)
        import base
            class (base(8)), intent(inout) :: dtv ! tcx: (8)
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module


subroutine unformattedRead (dtv, unit, iostat, iomsg)
use m, only: base
    class (base(8)), intent(inout) :: dtv ! tcx: (8)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (allocated (dtv%data)) deallocate (dtv%data)
 
    allocate (dtv%data(2))

    read (unit, iostat=iostat, iomsg=iomsg) dtv%data
end subroutine

program fdtio514a3kl
use m
use iso_fortran_env
    class (base(8)), pointer :: b1(:) ! tcx: (8)

    integer stat1
    character(200) err

    allocate (b1(2))

    open (1, access='stream', form='unformatted')

    write (1, pos=1) (1.0e0_8, 1.2e0_8), (2.1e0_8, 3.4e0_8)

    !! first read not from beginnig of the file
    read (1, pos=5, iostat=stat1, iomsg=err) b1(1)

    if (stat1 /= iostat_end) then
        print *, stat1, err
        error stop 101_4
    end if

    !! read more data than record
    read (1, pos=1, iostat=stat1, iomsg=err) b1

    if (stat1 /= iostat_end) then
        print *, stat1, err
        error stop 2_4
    end if
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (8) / declare with (8) - 3 changes
