! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : fdtio518kkkk
!*
!*  PROGRAMMER                 : David Forster (derived from fdtio518 by Jim Xia)
!*  DATE                       : 2007-08-16 (original: 01/12/2005)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO 
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : DTIO generics (multiple derived type components
!                               handled by DTIO)
!                               adaptation: exposed kinds
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
    type base1 (kbase1_1) ! kbase1_1=4
       integer, kind :: kbase1_1
        real(kbase1_1), pointer :: data(:)
    end type

    type base2 (kbase2_1) ! kbase2_1=4
       integer, kind :: kbase2_1
        complex(kbase2_1), allocatable :: data(:)
    end type


    interface write(formatted)
        subroutine formattedWriteBase1 (dtv, unit, iotype, v_list, iostat, iomsg)
        import base1
            class (base1(4)), intent(in) :: dtv ! tcx: (4)
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine

        subroutine formattedWriteBase2 (dtv, unit, iotype, v_list, iostat, iomsg)
        import base2
            class (base2(4)), intent(in) :: dtv ! tcx: (4)
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module


!! only deal with list-directed write
subroutine formattedWriteBase1 (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base1
    class (base1(4)), intent(in) :: dtv ! tcx: (4)
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (iotype /= 'LISTDIRECTED') return

    if (size(v_list) /= 0) error stop 11_4

    if (associated (dtv%data)) then
        write (unit, *, iostat=iostat, iomsg=iomsg) dtv%data
    end if
end subroutine

subroutine formattedWriteBase2 (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base2
    class (base2(4)), intent(in) :: dtv ! tcx: (4)
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (iotype /= 'LISTDIRECTED') return

    if (size(v_list) /= 0) error stop 21_4

    if (allocated (dtv%data)) then
        write (unit, *, iostat=iostat, iomsg=iomsg) dtv%data
    end if
end subroutine


module n
use m
    type dataType (kdataType_1,kdataType_2) ! kdataType_1,kdataType_2=4,4
       integer, kind :: kdataType_1,kdataType_2
        type (base1(kdataType_1)) :: b1 ! tcx: (kdataType_1)
        type (base2(kdataType_1)) :: b2 ! tcx: (kdataType_1)
    end type
end module


program fdtio518kkkk
use m
use n
    type (dataType(4,4)) :: d1(2) ! tcx: (4,4)

    allocate (d1(1)%b1%data(2), source=(/1.0, 2.0/))
    allocate (d1(1)%b2%data(-1:0), source= (/(3.0, 2.0), (4.0, 2.0)/))

    nullify(d1(2)%b1%data)

    allocate (d1(2)%b2%data(3), source=(/(1.3, 2.5), (2.3, 465.), (12.,1.2)/))

    !! test the write for scalar
    print *, d1(1)

    !! test the write for array
    print *, d1
end


! Extensions to introduce derived type parameters:
! type: base1 - added parameters (kbase1_1) to invoke with (4) / declare with (4) - 3 changes
! type: base2 - added parameters (kbase2_1) to invoke with (4) / declare with (4) - 3 changes
! type: dataType - added parameters (kdataType_1,kdataType_2) to invoke with (4,4) / declare with (4,4) - 1 changes
