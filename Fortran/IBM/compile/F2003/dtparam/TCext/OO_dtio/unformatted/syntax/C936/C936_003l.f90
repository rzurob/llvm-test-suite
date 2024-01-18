! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : C936_003l
!*
!*  PROGRAMMER                 : David Forster (derived from C936_003 by Robert Ma)
!*  DATE                       : 2007-09-09 (original: 11/04/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : Testing: CLASS(derived-type-spec) in DTIO subroutine
!*                                        shall be ILLEGAL for non-extensible type
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
    type base1 (lbase1_1) ! lbase1_1=3
       integer, len :: lbase1_1
        sequence
        character(lbase1_1) :: i
    end type
    
    type, bind(c) :: base2
        integer(4) :: i
    end type
    
end module

program C936_003l
use m

    interface read(unformatted)
    
        subroutine unformattedRead1 (dtv, unit, iostat, iomsg)
        use m, newbase1 => base1
            class(newbase1(*)), intent(inout) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
        
        subroutine unformattedRead2 (dtv, unit, iostat, iomsg)
        use m
            class(base2), intent(inout) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
        
    end interface
    
    interface write(unformatted)
        
        subroutine unformattedWrite1 (dtv, unit, iostat, iomsg)
        use m, newbase1 => base1
            class(newbase1(*)), intent(in) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
        
        subroutine unformattedWrite2 (dtv, unit, iostat, iomsg)
        use m
            class(base2), intent(in) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
        
    end interface

        
end program


subroutine unformattedRead1 (dtv, unit, iostat, iomsg)
use m, newbase1 => base1
    class(newbase1(*)), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    character(3) :: temp
 
    read (unit, iostat=iostat, iomsg=iomsg ) temp
       
    dtv%i = temp

end subroutine


subroutine unformattedWrite1 (dtv, unit, iostat, iomsg)
use m, newbase1 => base1
    class(newbase1(*)), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    
    write (unit, iostat=iostat, iomsg=iomsg ) dtv%i
       
end subroutine

subroutine unformattedRead2 (dtv, unit, iostat, iomsg)
use m
    class(base2), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    integer(4) :: temp
 
    read (unit, iostat=iostat, iomsg=iomsg ) temp
       
    dtv%i = temp

end subroutine

subroutine unformattedWrite2 (dtv, unit, iostat, iomsg)
use m, only: base2
    class(base2), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    
    write (unit, iostat=iostat, iomsg=iomsg ) dtv%i
       
end subroutine

! Extensions to introduce derived type parameters:
! type: base1 - added parameters (lbase1_1) to invoke with (3) / declare with (*) - 0 changes
