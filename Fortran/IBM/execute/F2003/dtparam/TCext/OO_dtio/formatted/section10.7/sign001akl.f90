! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : sign001akl
!*
!*  PROGRAMMER                 : David Forster (derived from sign001a by Robert Ma)
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
!*  DESCRIPTION                : Testing: Section 10.7.4: SS,SP, and S Editing
!*                                        Try these descriptors inside read DTIO procedures (no effect) with I, F editors
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

   type :: base (kb) ! kb=4
      integer, kind :: kb
      integer(kb):: i1
      real(kb)   :: r1
      complex(kb):: c1      
   end type
      
   interface write(formatted)
      subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base(4)), intent(in) :: dtv ! tcx: (4)
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
         class(base(4)), intent(inout) :: dtv ! tcx: (4)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)         
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine   
   end interface

end module

program sign001akl
   use m1   

   ! declaration of variables

   class(base(4)), allocatable  :: f1 ! tcx: (4)
   type(base(4)), pointer       :: f2(:,:) ! tcx: (4)
   type(base(4)) , allocatable  :: f3 ! tcx: (4)
   class(base(4)), pointer      :: f4(:) ! tcx: (4)
      
   integer :: stat
   character(200) :: msg
   
   open ( 1, file = 'sign001akl.1', form='formatted', access='sequential' )
   
   ! allocation of variables
   
   allocate (f1, source = base(4)(-1, 2.0, (3.0, -4.0) )) ! tcx: (4)
   allocate (f2(2,2), source = reshape (source=(/base(4)(1, -2.0, (-3.0, +4.0) ),base(4)(-5, +6.0, (7.0, -8.0) ),  & ! tcx: (4) ! tcx: (4)
                                                 base(4)(+9, +10.11, (+12.13, +14.15) ), base(4)(-16, 17.1819, (20.2122, 23.2425) ) /),shape=(/2,2/)) ) ! tcx: (4) ! tcx: (4)
   allocate (f3, source = f2(2,2) )
   allocate (f4(2), source = (/ f1, f3 /) )
   
   ! formatted I/O operations

   write (1, *, iostat=stat, iomsg=msg)                f1
   if ( ( stat /= 0  ) .or. ( msg /= 'dtiowrite' ) )   error stop 1_4
   
   write (1, *, iostat=stat, iomsg=msg)                f2   
   if ( ( stat /= 0  ) .or. ( msg /= 'dtiowrite' ) )   error stop 2_4

   write (1, *, iostat=stat, iomsg=msg)                f3
   if ( ( stat /= 0  ) .or. ( msg /= 'dtiowrite' ) )   error stop 3_4
    
   write (1, *, iostat=stat, iomsg=msg)                f4   
   if ( ( stat /= 0  ) .or. ( msg /= 'dtiowrite' ) )   error stop 4_4
   
   rewind 1
   
   deallocate ( f1, f2, f3, f4 )
   allocate (base(4):: f1, f2(2,2), f3, f4(2) ) ! tcx: base(4)
   
   read (1, *, iostat=stat, iomsg=msg)                       f1
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )          error stop 5_4
   
   read (1, *, iostat=stat, iomsg=msg)                       f2   
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )          error stop 6_4

   associate ( f13 => f3 )
      read (1, *, iostat=stat, iomsg=msg)                    f13
      if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )       error stop 7_4
   end associate
   
   select type ( f4 )    
      class default
         read (1, *, iostat=stat, iomsg=msg)                 f4   
         if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )    error stop 8_4
   end select
   
   write (6,*) f1
   write (6,*) f2
   write (6,*) f3
   write (6,*) f4
   
   close ( 1, status='delete' )
   
end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1, only: base
   class(base(4)), intent(inout) :: dtv ! tcx: (4)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)   
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read (unit, "(1X,SS,I4,1X,SS,F8.4,1X,SS,F8.3,1X,SS,F8.3)", iostat=iostat )      dtv%i1, dtv%r1, dtv%c1
       
   iomsg = 'dtioread'
       
end subroutine

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1, only: base
   class(base(4)), intent(in) :: dtv ! tcx: (4)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)   
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   
   write (unit, "(SS,'!',I4,1X,SP,F8.4,1X,S,F8.3,1X,SP,F8.3)", iostat=iostat )      dtv%i1, dtv%r1, dtv%c1
   
   iomsg = 'dtiowrite'
   
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb) to invoke with (4) / declare with (4) - 13 changes
