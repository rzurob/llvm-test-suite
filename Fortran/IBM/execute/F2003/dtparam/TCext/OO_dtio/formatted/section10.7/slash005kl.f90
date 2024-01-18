! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : slash005kl
!*
!*  PROGRAMMER                 : David Forster (derived from slash005 by Robert Ma)
!*  DATE                       : 2007-07-23 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Testing: Section 10.7.2: Slash Editing
!*                                        On output to a file connected for sequential, a new
!*                                        empty record is created following the current record.
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

module m1

   type :: base (lb) ! lb=3
      integer, len :: lb
      character(lb)   :: c1
   end type
      
   interface write(formatted)
      subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine   
   end interface

end module

program slash005kl
   use m1   

   ! declaration of variables

   class(base(:)), allocatable :: f1 ! tcx: (:)
      
   integer :: stat
   character(200) :: msg
   
   open ( 1, file = 'slash005kl.1', access = 'sequential', form='formatted' ) 
   
   ! allocation of variables
   
   allocate (f1, source = base(3)('ABC') ) ! tcx: (3)
     
   ! formatted I/O operations
   
   write ( 1, *, iostat = stat, iomsg = msg )    f1

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1, only: base
   class(base(*)), intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)   
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   
   character(20) :: format
   format = "(A,/)"
   write (unit, fmt=format, iostat=iostat )      dtv%c1
   
   iomsg = 'dtiowrite'
   
end subroutine

! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 4 changes
