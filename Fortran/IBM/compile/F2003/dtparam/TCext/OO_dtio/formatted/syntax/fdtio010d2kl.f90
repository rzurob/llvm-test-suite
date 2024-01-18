! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : fdtio010d2kl
!*
!*  PROGRAMMER                 : David Forster (derived from fdtio010d2 by Jim Xia)
!*  DATE                       : 2007-07-23 (original: 11/18/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO 
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : DTIO generics (syntax check for the formatted
!                               write; reverse the ordering of the dummy-arg)
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
    type point (kp) ! kp=4
       integer, kind :: kp
        real(kp) :: x, y
    end type
end module

program fdtio010d2kl
use m
    interface WRITE(FORMATTED)
        subroutine writePointFormatted (dtv, iotype, unit, v_list, iostat, iomsg)
        use m
            class (point(4)), intent(in) :: dtv ! tcx: (4)
            character(*), intent(in) :: iotype
            integer, intent(in) :: unit
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end



! Extensions to introduce derived type parameters:
! type: point - added parameters (kp) to invoke with (4) / declare with (4) - 1 changes
