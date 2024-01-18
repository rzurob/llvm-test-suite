! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-07-23 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.7.4: SS,SP, and S Editing
!*                                        Try these descriptors inside write DTIO procedures with G editors
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

   type :: base (kb,lb) ! kb,lb=4,2
      integer, kind :: kb
      integer, len :: lb
      integer(kb)   :: i1
      real(kb)      :: r1
      complex(kb)   :: c1
      character(lb) :: cc1
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

end module

program sign002kl
   use m1

   ! declaration of variables

   class(base(4,:)), allocatable  :: f1 ! tcx: (4,:)
   type(base(4,:)), pointer       :: f2(:,:) ! tcx: (4,:)
   type(base(4,:)) , allocatable  :: f3 ! tcx: (4,:)
   class(base(4,:)), pointer      :: f4(:) ! tcx: (4,:)

   integer :: stat
   character(200) :: msg

   open ( 1, file = 'sign002kl.1', form='formatted', access='sequential' )

   ! allocation of variables

   allocate (f1, source = base(4,2)(-1, 1.0, (2.0, -3.0),'AB' )) ! tcx: (4,2)
   allocate (f2(2,2), source = reshape (source=(/base(4,2)(1, -1.0, (-2.0, +3.0),'CD' ),base(4,2)(-5, +1.0, (2.0, -3.0),'EF' ),  & ! tcx: (4,2) ! tcx: (4,2)
                                                 base(4,2)(+9, +2.11, (+1.13, +3.15),'GH' ), base(4,2)(-16, 3.1819, (1.2122, 2.2425),'IJ' ) /),shape=(/2,2/)) ) ! tcx: (4,2) ! tcx: (4,2)
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


end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1, only: base
   class(base(4,*)), intent(in) :: dtv ! tcx: (4,*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write (unit, "(SS,G4,1X,SP,G8.2,1X,S,G8.2,1X,SP,G8.2,1X,SP,G4)", iostat=iostat )      dtv%i1, dtv%r1, dtv%c1, dtv%cc1

   iomsg = 'dtiowrite'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb) to invoke with (4) / declare with (4) - 11 changes
! type: base - added parameters (kb,lb) to invoke with (4,2) / declare with (4,*) - 11 changes
