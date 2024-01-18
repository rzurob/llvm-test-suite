! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : position004kl
!*
!*  PROGRAMMER                 : David Forster (derived from position004 by Robert Ma)
!*  DATE                       : 2007-07-22 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Testing: Section 10.7.1: Position Editing
!*                                        Try using non-advancing, pad mode is no, and use position editors to exceed
!*                                        the end-of-record.  and see if EOR branch can be taken
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
      character(lb)  :: c1 =''
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

   interface read(formatted)
      subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)         
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine   
   end interface  
   
end module

program position004kl
   use m1   
  
   ! declaration of variables

   class(base(:)), allocatable :: f1 ! tcx: (:)
   type(base(:)), pointer      :: f2(:) ! tcx: (:)
   type(base(:)) , allocatable :: f3 ! tcx: (:)
   class(base(:)) , pointer    :: f4(:) ! tcx: (:)
      
   integer :: stat
   character(200) :: msg
   
   open ( 2, file = 'position004kl.2', form='formatted', access='sequential', pad='no' )
   
   ! allocation of variables
   
   allocate (f1, source = base(3)('abc')) ! tcx: (3)
   allocate (f2(3), source = (/ base(3)('ABC'), base(3)('JKL'), base(3)('STU') /) ) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   allocate (base(3)::f3) ! tcx: base(3)
   allocate (base(3) :: f4(3)) ! tcx: (3)
     
   ! formatted I/O operations

   write (2, *, iostat=stat, iomsg=msg)                f1
   if ( ( stat /= 0  ) .or. ( msg /= 'dtiowrite' ) )   error stop 1_4
   write (2, *, iostat=stat, iomsg=msg)                f2(1:2)
   if ( ( stat /= 0  ) .or. ( msg /= 'dtiowrite' ) )   error stop 2_4
   
   rewind 2
   
   read (2, *, iostat=stat, iomsg=msg)                 f3
   if (  msg /= 'myeor'  )                             error stop 3_4 
   
   read (2, *, iostat=stat, iomsg=msg)                 f4
   if (  msg /= 'myeor'  )                             error stop 4_4 
      
   ! close the file appropriately
   
   close ( 2, status ='delete' )
      
end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1, only: base
   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)   
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   
20 format(T15,A3)

   read ( unit, 20, iostat=iostat, advance='no', EOR=70 )                    dtv%c1
   error stop 5_4
70 read ( unit, "(T5,2X,A3)", iostat=iostat, advance='no', EOR=80 )          dtv%c1
   error stop 6_4
   
80 iomsg = 'myeor'
end subroutine

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1, only: base
   class(base(*)), intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)   
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   
10 format ( A3 )
   write (unit, 10, iostat=iostat )                    dtv%c1
  
   iomsg = 'dtiowrite'
   
end subroutine

! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 13 changes
