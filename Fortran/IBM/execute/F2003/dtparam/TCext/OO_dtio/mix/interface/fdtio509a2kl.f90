! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : fdtio509a2kl
!*
!*  PROGRAMMER                 : David Forster (derived from fdtio509a2 by Jim Xia)
!*  DATE                       : 2007-08-14 (original: 03/03/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)

!*  DRIVER STANZA              : xlf2003

!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : DTIO generics (ENDFILE statement not allowed in
!                               DTIO)
!                               adaptation: exposed kind
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (kbase_1) ! kbase_1=8
       integer, kind :: kbase_1
        complex(kbase_1), pointer :: data => null()
    end type

    interface write(unformatted)
        subroutine unformattedWrite (dtv, unit, iostat, iomsg)
        import base
            class (base(8)), intent(in) :: dtv ! tcx: (8)
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module

program fdtio509a2kl
use m
    type (base(8)) :: b1(2) ! tcx: (8)

    integer stat1
    character(200) err

    allocate (b1(1)%data, source=(1.0_8,2.9_8))

    open (1, file='fdtio509a2kl.data', form='unformatted')

    write (1, iostat=stat1, iomsg=err) b1   !<-- should fail

    if (stat1 /= 0) then
        print *, stat1, err
        error stop 2_4
    end if

    close(1, status='delete')
end

subroutine unformattedWrite (dtv, unit, iostat, iomsg)
use m, only: base
    class (base(8)), intent(in) :: dtv ! tcx: (8)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (associated (dtv%data)) then
        write (unit, iostat=iostat, iomsg=iomsg) dtv%data

        flush unit
    else
        endfile unit  !<-- illegal
    end if
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (8) / declare with (8) - 3 changes
! type: base - added parameters (kbase_1) to invoke with (8) / declare with (8) - 3 changes
