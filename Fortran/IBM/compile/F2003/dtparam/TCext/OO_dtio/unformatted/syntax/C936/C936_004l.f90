! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : C936_004l
!*
!*  PROGRAMMER                 : David Forster (derived from C936_004 by Robert Ma)
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
!*  DESCRIPTION                : Testing: TYPE(derived-type-spec) in DTIO subroutine
!*                                        shall be ILLEGAL for extensible type
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
    type :: base (lbase_1) ! lbase_1=3
       integer, len :: lbase_1
        character(lbase_1) :: i 
    end type    
end module

program C936_004l
use m

    interface read(unformatted)
        subroutine unformattedRead (dtv, unit, iostat, iomsg)
        use m, newbase => base
            type(newbase(*)), intent(inout) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
    
    interface write(unformatted)
        subroutine unformattedWrite (dtv, unit, iostat, iomsg)
        use m, newbase => base
            type(newbase(*)), intent(in) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

end program


subroutine unformattedRead (dtv, unit, iostat, iomsg)
use m, newbase1 => base
    type(newbase1(*)), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    character(3) :: temp
 
    read (unit, iostat=iostat, iomsg=iomsg ) temp
        
    dtv%i = temp

   
end subroutine


subroutine unformattedWrite (dtv, unit, iostat, iomsg)
use m
    type(base(*)), intent(in) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    
    write (unit, iostat=iostat, iomsg=iomsg ) dtv%i
       
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 1 changes
