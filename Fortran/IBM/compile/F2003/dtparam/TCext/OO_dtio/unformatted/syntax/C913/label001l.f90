! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : label001l
!*
!*  PROGRAMMER                 : David Forster (derived from label001 by Robert Ma)
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
!*  DESCRIPTION                : Testing: Section 9.5 Data Transfer Statement
!*                               C913: label shall exist and label shall be in the same scope unit
!*                               - Test "ERR="
!*                                 - label does not exist
!*                                 - label in another scoping unit
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
      contains
         procedure, pass :: getC
         procedure, pass :: setC
   end type
contains
   function getC (a)
      class(base(*)), intent(in) :: a ! tcx: (*)
      character(3) :: getC
      getC = a%c      
   end function   
   
   subroutine setC (a, char)
      class(base(*)), intent(inout) :: a ! tcx: (*)
      character(3), intent(in) :: char      
      a%c = char
   end subroutine   
end module


program label001l
   use m1   
   
   interface read(formatted)
      subroutine readFormatted (dtv, unit, iotype, v_list, iostat, iomsg)
      import base
         class (base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface
    
   interface write(formatted)
      subroutine writeFormatted (dtv, unit, iotype, v_list, iostat, iomsg)
      import base
         class (base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)            
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface   
   
   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(*)),  intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout):: iomsg
      end subroutine
   end interface

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine   
   end interface
   
   integer :: stat
   character(100) :: msg
   
   class(base(:)), allocatable :: b1 ! tcx: (:)
   class(base(:)), pointer :: b2 ! tcx: (:)

   allocate (b1, source = base(3)("ibm") ) ! tcx: (3)
   allocate (b2, source = base(3)("IBM") ) ! tcx: (3)

   open (1, file="label001l.udata", form="unformatted", access="sequential" )

   write (1, err=500, iostat=stat, iomsg=msg ) b1       !<= label does not exist
   write (1, err=800, iostat=stat, iomsg=msg ) b1       !<= label exists and is select-type-stmt
   
   call mywriteunformatted (1, b1)
   
   rewind 1

   read (1, err=500, iostat=stat, iomsg=msg )     b2    !<= label does not exist
   read (1, err=800, iostat=stat, iomsg=msg )     b2    !<= label exists and is select-type-stmt

300 print *, "This is a out of scope label for subroutines"

800 select type ( b2 )
      class is (base(*)) ! tcx: (*)
         call myreadunformatted   (1, b1)
    end select   

contains

   subroutine mywriteunformatted (unit, a)
      integer, intent(in) :: unit
      class(base(*)), intent(in) :: a ! tcx: (*)
      write (unit, err=300) a                            !<= label out of scope
   end subroutine

   subroutine myreadunformatted (unit, a)
      integer, intent(in) :: unit
      class(base(*)), intent(inout) :: a ! tcx: (*)
      character(3) :: temp
      read (unit, err=300) a                             !<= label out of scope
   end subroutine

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base(*)), intent(inout) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character, intent(inout) :: iomsg

    character(3) :: temp
    read (unit, iostat=iostat, iomsg=iomsg ) temp
    
    call dtv%setC(temp)
    
end subroutine

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base(*)), intent(in) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character, intent(inout) :: iomsg
    
    write (unit, iostat=iostat, iomsg=iomsg ) dtv%getC()
    
end subroutine

subroutine readFormatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1
    class(base(*)), intent(inout) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in)  :: v_list(:) 
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    character(3) :: temp
    read (unit, *, iostat=iostat, iomsg=iomsg) temp
    call dtv%setC(temp)
    
end subroutine


subroutine writeFormatted (dtv, unit, iotype, v_list,iostat, iomsg)
use m1
    class(base(*)), intent(in) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in)  :: v_list(:)     
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
      
    write (unit, *, iostat=iostat, iomsg=iomsg) dtv%getC()
    write (unit, *, iostat=iostat, iomsg=iomsg ) " "     !<- insert space between records

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 17 changes
