! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : fdtio051kl
!*
!*  PROGRAMMER                 : David Forster (derived from fdtio051 by Jim Xia)
!*  DATE                       : 2007-06-19 (original: 11/17/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO 
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : DTIO generics (list directed write should add
!                               value separators by parent data transfer)
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
    type A (k)
       integer, kind :: k
       integer(k), pointer :: i => null()
    end type

    interface write (formatted)
        subroutine abc (dtv, unit, iotype, v_list, iostat, iomsg)
        import A
            class (A(4)), intent(in) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module

program fdtio051
use m
    type (A(4)) a1(3)

    do i = 1, 3
        allocate (a1(i)%i, source=i)
    end do

    print *, a1

    write (*,*) a1(2), a1(1)
end


subroutine abc (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only : A
    class (A(4)), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (associated(dtv%i)) write (unit, '(i1)', iostat=iostat, &
                                    iomsg=iomsg) dtv%i
end subroutine

