! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : fdtio514a1kl
!*
!*  PROGRAMMER                 : David Forster (derived from fdtio514a1 by Jim Xia)
!*  DATE                       : 2007-08-16 (original: 01/10/2005)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO 
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : DTIO generics (end of file encountered while
!                               reading internal file)
!*                               adaptation: exposed kind, length
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
        integer(kbase_1), allocatable :: id

        contains

        procedure :: read => readBase
    end type

    type, extends(base) :: child (lchild_1) ! lchild_1=20
       integer, len :: lchild_1
        character(lchild_1), allocatable :: name

        contains

        procedure :: read => readChild
    end type

    character(*), parameter :: separator = '|'

    contains

    subroutine readBase (b, unit, iotype, vlist, iostat, iomsg)
        class (base(8)), intent(inout) :: b ! tcx: (8)
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: vlist(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        !! we only deal with list-directed

        if (iotype /= 'LISTDIRECTED') then
            error stop 10_4
        end if

        if (size(vlist) /= 0) error stop 11_4


        if (.not. allocated (b%id)) allocate (b%id)

        read (unit, *, iostat=iostat, iomsg=iomsg) b%id
    end subroutine


    subroutine readChild (b, unit, iotype, vlist, iostat, iomsg)
        class (child(8,*)), intent(inout) :: b ! tcx: (8,*)
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: vlist(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (iotype /= 'LISTDIRECTED') then
            error stop 20_4
        end if

        if (size(vlist) /= 0) error stop 21_4

        if (.not. allocated (b%id)) allocate (b%id)

        if (.not. allocated (b%name)) allocate (b%name)

        read (unit, *, iostat=iostat, iomsg=iomsg) b%id, b%name
    end subroutine
end module


module n
    interface read (formatted)
        subroutine readFormatted(dtv, unit, iotype, vlist, iostat, iomsg)
        use m, only: base
            class (base(8)), intent(inout) :: dtv ! tcx: (8)
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: vlist(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module


subroutine readFormatted(dtv, unit, iotype, vlist, iostat, iomsg)
use m, only: base
    class (base(8)), intent(inout) :: dtv ! tcx: (8)
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    call dtv%read (unit, iotype, vlist, iostat, iomsg)
end subroutine

program fdtio514a1kl
use m
use n
use iso_fortran_env
    class (base(8)), allocatable :: b1(:), b2(:) ! tcx: (8)

    character(20) data1
    character(10) data2

    integer stat1
    character(200) err

    data1 = '12345, abcdefghxyz1'

    allocate (child(8,20):: b1(2)) ! tcx: (8,20)

    !! this read will fail due to EOF
    read (data1, *, iostat=stat1, iomsg=err) b1

   if (stat1 /= iostat_end) then
       print *, stat1, err
       error stop 101_4
   end if

    allocate (b2(10))


    data2 = '10, 11, 12'

    !! the read stmt will fail due to EOF condition
    read (data2, *, iostat=stat1, iomsg=err) b2(1:6)

    if (stat1 /= iostat_end) then
        print *, stat1, err
        error stop 2_4
    end if
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (8) / declare with (8) - 4 changes
! type: child - added parameters (lchild_1) to invoke with (8,20) / declare with (8,*) - 2 changes
