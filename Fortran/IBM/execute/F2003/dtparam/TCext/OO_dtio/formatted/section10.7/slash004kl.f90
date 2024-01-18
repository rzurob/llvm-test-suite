! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : slash004kl
!*
!*  PROGRAMMER                 : David Forster (derived from slash004 by Robert Ma)
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
!*                                        Try slash editing on internal file
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

   type :: base (lb1,lb2) ! lb1,lb2=3,2
      integer, len :: lb1,lb2
      character(lb1)   :: c1
      character(lb1)   :: c2(lb2)
   end type
      
   interface write(formatted)
      subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base(*,*)), intent(in) :: dtv ! tcx: (*,*)
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
         class(base(*,*)), intent(inout) :: dtv ! tcx: (*,*)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)         
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine   
   end interface  
   
end module

program slash004kl
   use m1   

   ! declaration of variables

   class(base(:,:)), allocatable :: f1 ! tcx: (:,:)
   type(base(:,:)), pointer      :: f2(:) ! tcx: (:,:)
   type(base(:,:)) , allocatable :: f3 ! tcx: (:,:)
   class(base(:,:)), pointer     :: f4(:) ! tcx: (:,:)
      
   integer :: stat
   character(200) :: msg
   
   character(4) :: internalFile(20)
   
   ! allocation of variables
   
   allocate (f1, source = base(3,2)('ABC', (/'DEF','GHI'/))) ! tcx: (3,2)
   allocate (f2(2), source = (/ base(3,2)('abc', (/'def','ghi'/)), base(3,2)('jkl', (/'mno','pqr'/)) /) ) ! tcx: (3,2) ! tcx: (3,2)
   allocate (base(3,2)::f3) ! tcx: base(3,2)
   allocate (base(3,2) :: f4(2)) ! tcx: (3,2)
     
   ! formatted I/O operations

   write (internalFile(3:1:-1), *, iostat=stat, iomsg=msg)         f1
   if ( ( stat /= 0  ) .or. ( msg /= 'dtiowrite' ) )   error stop 1_4
   
   write (internalFile(4:10), *, iostat=stat, iomsg=msg)           f2(1:2)   
   if ( ( stat /= 0  ) .or. ( msg /= 'dtiowrite' ) )   error stop 2_4
      
   read (internalFile(4:9), *, iostat=stat, iomsg=msg)             f4
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )     error stop 3_4
   read (internalFile(3:1:-1), *, iostat=stat, iomsg=msg)          f3
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )     error stop 4_4
        
   ! check if values are read correctly

   if ( (  f3%c1 /= 'ABC' ) .or. (  f3%c2(1) /= 'DEF' )  .or. (  f3%c2(2) /= 'GHI' )  )               error stop 5_4
   if ( (  f4(1)%c1 /= 'abc' ) .or. (  f4(1)%c2(1) /= 'def' ) .or. (  f4(1)%c2(2) /= 'ghi' ) .or. &
        (  f4(2)%c1 /= 'jkl' ) .or. (  f4(2)%c2(1) /= 'mno' ) .or. (  f4(2)%c2(2) /= 'pqr' )  )       error stop 6_4
      
end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1, only: base
   class(base(*,*)), intent(inout) :: dtv ! tcx: (*,*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)   
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

10 format (A,/,1X,A,/,1X,A,/)
   read (unit, 10, iostat=iostat )             dtv%c1, dtv%c2
 
   iomsg = 'dtioread'
       
end subroutine

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1, only: base
   class(base(*,*)), intent(in) :: dtv ! tcx: (*,*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)   
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   
   character(20) :: format
   format = "(A3,/,1X,A,/,1X,A,/)"
   write (unit, fmt=format, iostat=iostat )      dtv%c1, dtv%c2
   
   iomsg = 'dtiowrite'
   
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb1,lb2) to invoke with (3,2) / declare with (*,*) - 12 changes
