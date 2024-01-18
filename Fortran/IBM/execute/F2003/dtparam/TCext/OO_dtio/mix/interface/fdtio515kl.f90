! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : fdtio515kl
!*
!*  PROGRAMMER                 : David Forster (derived from fdtio515 by Jim Xia)
!*  DATE                       : 2007-08-16 (original: 05/31/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*  DRIVER STANZA              : xlf2003
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : DTIO generics (null value effects on
!                               list-directed DTIO read)
!                               adaptation: exposed kind
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
        real(kbase_1), allocatable :: data
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

    if (allocated (dtv%data)) deallocate (dtv%data)

    allocate (dtv%data, source=-1.0)

    read (unit, *, iostat=iostat, iomsg=iomsg) dtv%data
end subroutine


program fdtio515kl
use m
    class (base(4)), allocatable :: b1(:,:) ! tcx: (4)

    allocate (b1(2,2))

    allocate (b1(1,1)%data, b1(1,2)%data, b1(2,2)%data)

    open (10, file= 'fdtio515kl.data')

    write (10, *) '1.2, 2.1, 3*, 4.5, 2.3'
    write (10, *) '2.2, /'
    write (10, *) ' 5.5, 2.3'

    rewind (10)

    read (10, *) b1

    write (*, '(4(f10.2))') b1(1,1)%data, b1(2,1)%data, b1(1,2)%data, b1(2,2)%data

    read (10, *) b1

    write (*, '(4(f10.2))') ((b1(i,j)%data, i=1,2), j=1,2)

    close (10, status='delete')
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 3 changes
