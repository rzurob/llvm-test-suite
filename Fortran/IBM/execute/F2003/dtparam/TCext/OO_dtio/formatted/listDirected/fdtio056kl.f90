! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : fdtio056kl
!*
!*  PROGRAMMER                 : David Forster (derived from fdtio056 by Jim Xia)
!*  DATE                       : 2007-06-19 (original: 11/30/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO 
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : DTIO generics (handle the inheritance
!                               relationship in DTIO)
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
    type base (kb) ! kb=8
       integer, kind :: kb
        real(kb) r1
    end type

    type, extends(base) :: child (kc,lc) ! kc,lc=8,15
       integer, kind :: kc
       integer, len :: lc
        character(lc) :: name
        integer(kc) :: id
    end type

    interface write(formatted)
        subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
        import base
            class (base(8)), intent(in) :: dtv ! tcx: (8)
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

end module

program fdtio056kl
use m
    class (base(8)), allocatable :: b1(:) ! tcx: (8)

    type (child(8,8,15)) c1(3) ! tcx: (8,8,15)

    c1%r1 = (/(2.0*i, i=1,3)/)

    c1%name = (/'c1_1', 'c1_2', 'c1_3'/)
    c1%id = (/3, 2, 1/)

    allocate (b1(0:2), source=c1)

    print *, c1

    write(*,*) b1
end


subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only:base, child
    class (base(8)), intent(in) :: dtv ! tcx: (8)
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, '(a, g9.2)', iostat=iostat, iomsg=iomsg) 'r1=', dtv%r1

    if (iostat /= 0) return

    select type (dtv)
        type is (child(8,8,*)) ! tcx: (8,8,*)
            write (unit, *, iostat=iostat, iomsg=iomsg) 'name=', dtv%name, &
                    'id=', dtv%id, "|"
        type is (base(8)) ! tcx: (8)

        class default
            error stop 10_4
    end select
end subroutine
