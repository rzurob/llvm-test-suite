! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : fdtio508a1kl
!*
!*  PROGRAMMER                 : David Forster (derived from fdtio508a1 by Jim Xia)
!*  DATE                       : 2007-08-14 (original: 01/31/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : DTIO generics (test the unformatted read in
!                               select type construct)
!                               adaptation: exposed kind
!                               (empty "container" adaptation is uninteresting)
!*
!*  KEYWORD(S)                 :
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
        integer(kbase_1), allocatable :: id(:)
    end type

    interface read(unformatted)
        subroutine unformattedRead (dtv, unit, iostat, iomsg)
        import base
            class (base(4)), intent(inout) :: dtv ! tcx: (4)
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module


subroutine unformattedRead (dtv, unit, iostat, iomsg)
use m, only: base
    class (base(4)), intent(inout) :: dtv ! tcx: (4)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    integer size1

    if (allocated (dtv%id)) deallocate (dtv%id)

    !! read size and then the array
    read (unit, iostat=iostat, iomsg=iomsg) size1

    if (iostat /= 0) return

    if (size1 >= 0) then
        allocate (dtv%id(size1))

        read (unit, iostat=iostat, iomsg=iomsg) dtv%id
    end if
end subroutine


program fdtio508a1kl
use m
    integer stat1
    character(200) err

    type container
        class (*), pointer :: data(:)
    end type

    type (container) :: co1

    open (1, file='fdtio508a1kl.data', form='unformatted')

    write (1) 2, 10, 15

    write (1) 3, 15, 5, 3, 2, 2, 3

    rewind (1)

    allocate (base(4):: co1%data(2)) ! tcx: (4)

    select type (y => co1%data)
        class is (base(4)) ! tcx: (4)
            !! test the read for scalar
            read (1, iostat=stat1, iomsg=err) y(1)

            if (stat1 /= 0) then
                print *, stat1, err
                error stop 5_4
            end if

            if (size (y(1)%id) /= 2) error stop 6_4

            if (any (y(1)%id /= (/10, 15/))) error stop 7_4

            !! test read for array
            read (1, iostat=stat1, iomsg=err) y

            if (stat1 /= 0) then
                print *, stat1, err
                error stop 8_4
            end if

            if ((size (y(1)%id) /= 3) .or. (size (y(2)%id) /= 2)) error stop 9_4

            if (any (y(1)%id /= (/15,5,3/))) error stop 10_4

            if (any (y(2)%id /= (/2,3/))) error stop 11_4
        class default
            error stop 101_4
    end select

    close (1, status='delete')
end


! Extensions to introduce derived type parameters:
! type: base - added parameters () to invoke with () / declare with () - 4 changes
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 4 changes
