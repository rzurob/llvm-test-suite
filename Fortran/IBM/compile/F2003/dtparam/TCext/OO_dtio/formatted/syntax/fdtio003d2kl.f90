! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : fdtio003d2kl
!*
!*  PROGRAMMER                 : David Forster (derived from fdtio003d2 by Jim Xia)
!*  DATE                       : 2007-07-23 (original: 11/19/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO 
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : DTIO generics (DTIO defined for child type can
!                               not be used for data declared to be base type)
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
    type base (lb) ! lb=5
       integer, len :: lb
        character(lb) :: data
    end type

    type, extends(base) :: child (kb) ! kb=4
       integer, kind :: kb
        integer(kb), allocatable :: i1
    end type

    interface write(formatted)
        subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
        import child
            class (child(*,4)), intent(in) :: dtv ! tcx: (*,4)
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module

program fdtio003d2kl
use m
    class (base(:)), allocatable :: b1 ! tcx: (:)

    allocate (b1, source=child(5,4)('abcde', 100)) ! tcx: (5,4)

    print *, b1  !<-- illegal call
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (5) / declare with (*) - 1 changes
! type: child - added parameters (kb) to invoke with (5,4) / declare with (*,4) - 2 changes
