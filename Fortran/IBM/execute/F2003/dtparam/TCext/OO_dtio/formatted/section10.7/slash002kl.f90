! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : slash002kl
!*
!*  PROGRAMMER                 : David Forster (derived from slash002 by Robert Ma)
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
!*                                        Try slash editing with r inside DTIO
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

   type :: base (kb,lb) ! kb,lb=4,3
      integer, kind :: kb
      integer, len :: lb
      integer(kb)    :: c1
      real(kb)       :: c2
      character(lb)  :: c3
   end type
      
   interface write(formatted)
      subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base(4,*)), intent(in) :: dtv ! tcx: (4,*)
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
         class(base(4,*)), intent(inout) :: dtv ! tcx: (4,*)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)         
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine   
   end interface  
   
end module

program slash002kl
   use m1   
   
   procedure(logical) :: precision_r4
   ! declaration of variables

   class(base(4,:)), allocatable :: f1 ! tcx: (4,:)
   type(base(4,:)), pointer      :: f2(:) ! tcx: (4,:)
   type(base(4,:)) , allocatable :: f3 ! tcx: (4,:)
   class(base(4,:)), pointer     :: f4(:) ! tcx: (4,:)
      
   integer :: stat
   character(200) :: msg
   
   open ( unit = 1, file = 'slash002kl.1', access='stream', form='formatted' )
   
   ! allocation of variables
   
   allocate (f1, source = base(4,3)(1,2.0,'abc')) ! tcx: (4,3)
   allocate (f2(2), source = (/ base(4,3)(2,3.0,'def'), base(4,3)(4,5.0,'ghi') /) ) ! tcx: (4,3) ! tcx: (4,3)
   allocate (base(4,3)::f3) ! tcx: base(4,3) ! tcx: base(4,3)
   allocate (base(4,3) :: f4(2)) ! tcx: (4,3)
     
   ! formatted I/O operations

   write (1, *, iostat=stat, iomsg=msg)                f1
   if ( ( stat /= 0  ) .or. ( msg /= 'dtiowrite' ) )   error stop 1_4
   write (1, *, iostat=stat, iomsg=msg)                f2
   if ( ( stat /= 0  ) .or. ( msg /= 'dtiowrite' ) )   error stop 2_4
   
   rewind 1
   
   read (1, *, iostat=stat, iomsg=msg)                 f3
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )    error stop 3_4
   read (1, *, iostat=stat, iomsg=msg)                 f4
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )    error stop 4_4
   
   print *, f3
   print *, f4
      
   ! check if values are read correctly
   
   if ( (  f3%c1 /= 1 ) .or. ( .not. precision_r4(f3%c2,2.0) ) .or. (  f3%c3 /= 'abc' ) )   error stop 5_4
   if ( (  f4(1)%c1 /= 2 ) .or. ( .not. precision_r4(f4(1)%c2,3.0)) .or. (  f4(1)%c3 /= 'def' ) .or. &
        (  f4(2)%c1 /= 4 ) .or. ( .not. precision_r4(f4(2)%c2,5.0)) .or. (  f4(2)%c3 /= 'ghi' ) )   error stop 6_4
   
   ! close the file appropriately
   
   close ( 1, status ='delete' )
      
end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1, only: base
   class(base(4,*)), intent(inout) :: dtv ! tcx: (4,*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)   
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

10 format ( I1,3/,/,1X,F5.2 )
   read (unit, 10, iostat=iostat )                      dtv%c1, dtv%c2
   if (iostat /= 0 )   error stop 9_4
   read (unit, "(/,1X,A3,/)", iostat = iostat )     dtv%c3
   
   iomsg = 'dtioread'
       
end subroutine

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1, only: base
   class(base(4,*)), intent(in) :: dtv ! tcx: (4,*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)   
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   
   character(17) :: format
   format = "(I4,3/,/,1X,F5.2)"
   write (unit, fmt=format, iostat=iostat )             dtv%c1, dtv%c2
   if (iostat /= 0 )   error stop 9_4
   write (unit, "(/,1X,A3,/)", iostat = iostat )        dtv%c3
   
   iomsg = 'dtiowrite'
   
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb,lb) to invoke with (4,3) / declare with (4,*) - 12 changes
