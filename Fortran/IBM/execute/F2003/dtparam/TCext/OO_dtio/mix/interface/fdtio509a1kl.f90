! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : fdtio509a1kl
!*
!*  PROGRAMMER                 : David Forster (derived from fdtio509a1 by Jim Xia)
!*  DATE                       : 2007-08-14 (original: 03/03/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : DTIO generics (test the compiler behavior of
!                               recursive IO using rewind; current behavior is
!                               SIGABRT)
!                               adaptation: exposed kind
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (kbase_1) ! kbase_1=8
       integer, kind :: kbase_1
        integer(kbase_1), pointer :: data
    end type

    character, parameter :: ALLOC = 'A'
    character, parameter :: UNALLOC = 'U'
end module

program fdtio509a1kl
use m
    interface write(unformatted)
        subroutine unformattedWrite (dtv, unit, iostat, iomsg)
        use m
            class (base(8)), intent(in) :: dtv ! tcx: (8)
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    integer stat
    character(200) msg

    write (1, iostat=stat, iomsg=msg) base(8)(null()) ! tcx: (8)
end


subroutine unformattedWrite (dtv, unit, iostat, iomsg)
use m
    class (base(8)), intent(in) :: dtv ! tcx: (8)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (associated (dtv%data)) then
        write (unit, iostat=iostat, iomsg=iomsg)  ALLOC, dtv%data
    else
        write (unit, iostat=iostat, iomsg=iomsg)  UNALLOC
    end if

    if (iostat /= 0) return

    flush (unit)

    rewind(unit, iostat=iostat, iomsg=iomsg)    ! this is illegal operation
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (8) / declare with (8) - 3 changes
