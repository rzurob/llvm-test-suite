! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArgCharctrstc009k
!*
!*  PROGRAMMER                 : David Forster (derived from dummyArgCharctrstc009 by Robert Ma)
!*  DATE                       : 2007-09-09 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : Testing: Characteristics of DTIO interface and procedures
!*                               shall be the same as ones defined in Section 9.5.3.7.2.
!*                               - Dummy Argument Characteristics 
!*                                  - with different dummy argument names (compiler should not complain)
!*                                    and ensure iostat and iomsg still works (with unformatted I/O)
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1

   type base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      character(lbase_1) :: c
   end type
      
end module

program dummyArgCharctrstc009k
   use m1
   
   interface read(unformatted)
      subroutine readUnformatted (dtv2, unit2, iostat2, iomsg2)
         import base
         class(base(*)),  intent(inout) :: dtv2 ! tcx: (*)
         integer, intent(in) :: unit2
         integer, intent(out) :: iostat2
         character(*), intent(inout):: iomsg2
      end subroutine
   end interface
   
   interface write(unformatted)
      subroutine writeUnformatted (dtv3, unit3, iostat3, iomsg3)
         import base
         class(base(*)), intent(in) :: dtv3 ! tcx: (*)
         integer,  intent(in) :: unit3
         integer,  intent(out) :: iostat3
         character(*),  intent(inout) :: iomsg3
      end subroutine
   end interface   
   
   class(base(:)), allocatable :: b1, b2 ! tcx: (:)
   integer :: stat
   character(100) :: msg  = ""
   character(100) :: msg1 = ""
   
   allocate ( b1, source = base(3)("IBM") ) ! tcx: (3)
   allocate ( b2, source = base(3)("   ") ) ! tcx: (3)
   
   open (1, file="dummyArgCharctrstc009k.data", form="unformatted") 
   
   write (1, iostat = stat, iomsg = msg) b1
   
   if ( stat /= 0 )       error stop 101_4
   if ( msg  /= "")       error stop 2_4
   
   read  (1, iostat = stat, iomsg = msg) b2        !<- should have end of file error
   
   if ( stat /= -1 )      error stop 3_4
   if ( msg  == "" )      error stop 4_4
   
   rewind 1
   
   msg1 = msg
   
   read  (1, iostat = stat, iomsg = msg1) b2       !<- successful read shall not change msg1
      
   if ( stat /= 0 )       error stop 5_4
   if ( msg1 /= msg)      error stop 6_4
   
   ! close the file appropriately
   
   close ( 1, status ='delete' )
   
end program

subroutine readUnformatted (mydtv, myunit, myiostat, myiomsg)
use m1, only:base
    class(base(*)), intent(inout) :: mydtv ! tcx: (*)
    integer, intent(in) :: myunit
    integer, intent(out) :: myiostat
    character, intent(inout) :: myiomsg

    character(3) :: temp
    read (myunit, iostat=myiostat, iomsg=myiomsg ) temp
    mydtv%c = temp  
end subroutine

subroutine writeUnformatted (mydtv, myunit, myiostat, myiomsg)
use m1, only:base
    class(base(*)), intent(in) :: mydtv ! tcx: (*)
    integer, intent(in) :: myunit
    integer, intent(out) :: myiostat
    character, intent(inout) :: myiomsg
    
    write (myunit, iostat=myiostat, iomsg=myiomsg ) mydtv%c
      
end subroutine

! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 7 changes
