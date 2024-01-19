! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-07-23 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.7.5: P Editing
!*                                        Inside DTIO, P edit and see if the connection has changed scale factor temporarily for write
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
      integer(kb)   :: i1(lb)
      real(kb)      :: r1(lb)
      complex(kb)   :: c1(lb)
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

program scale001kl
   use m1

   ! declaration of variables

   class(base(4,:)), allocatable  :: f1 ! tcx: (4,:)
   type(base(4,:)), pointer       :: f2(:,:) ! tcx: (4,:)
   type(base(4,:)) , allocatable  :: f3 ! tcx: (4,:)
   class(base(4,:)), pointer      :: f4(:) ! tcx: (4,:)

   integer :: stat
   character(200) :: msg

   open ( 1, file = 'scale001kl.1', form='formatted', access='sequential' )

   ! allocation of variables

   allocate (f1, source = base(4,2)( (/111,222/), (/1.11,2.22/), (/ (33.3, 44.4), (55.5, 66.6) /) )) ! tcx: (4,2)
   allocate (f2(2,2), source = reshape (source=(/ base(4,2)( (/333,444/),  (/5.55,6.66/), (/ (77.7, 88.8), (99.9, 11.1) /) ),  & ! tcx: (4,2)
                                                  base(4,2)( (/555,666/),  (/7.77,8.88/), (/ (99.9, 11.1), (22.2, 33.3) /) ),  & ! tcx: (4,2)
                                                  base(4,2)( (/777,888/),  (/9.99,1.11/), (/ (22.2, 33.3), (99.9, 44.4) /) ),  & ! tcx: (4,2)
                                                  base(4,2)( (/999,111/),  (/2.22,3.33/), (/ (44.4, 55.5), (66.6, 77.7) /) )   & ! tcx: (4,2)
                                                /),shape=(/2,2/)) )
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

   write ( unit, iostat = iostat, fmt = '(-1P,I4,1X,I4 )')           dtv%i1(1), dtv%i1(2)
   if ( iostat /= 0 ) error stop 9_4
   write ( unit, iostat = iostat, fmt = '(1X,F8.4,-1P,1X,F8.4 )')   dtv%r1(1), dtv%r1(2)
   if ( iostat /= 0 ) error stop 10_4
   write ( unit, iostat = iostat, fmt = '(1X,F8.4,-3P,1X,F8.4,0P,1X,F8.4,1P,1X,F8.4 )')   dtv%c1(1), dtv%c1(2)

   iomsg = 'dtiowrite'

end subroutine



! Extensions to introduce derived type parameters:
! type: base - added parameters (kb,lb) to invoke with (4,2) / declare with (4,*) - 11 changes
