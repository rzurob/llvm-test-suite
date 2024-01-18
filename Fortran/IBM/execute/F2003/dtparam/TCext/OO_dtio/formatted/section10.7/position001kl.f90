! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-07-22 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.7.1: Position Editing
!*                                        Try different position editing descriptor in child data transfer stmt
!*                                        First try T, TL, TR, X edit descriptors together with I,A,F data editors
!*                                        with sequential access
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
   type base (kb,lb) ! kb,lb=4,1
      integer, kind :: kb
      integer, len :: lb
      real(kb)      :: r
      integer(kb)   :: i
      character(lb) :: c
   end type

end module


program position001kl
   use m1

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

   ! declaration of variables

   procedure(logical) :: precision_r4
   class(base(4,:)), allocatable :: f1 ! tcx: (4,:)
   type(base(4,:)), pointer     :: f2(:) ! tcx: (4,:)
   type(base(4,:)) , allocatable :: f3 ! tcx: (4,:)
   class(base(4,:)) , pointer     :: f4(:) ! tcx: (4,:)

   integer :: stat
   character(200) :: msg

   ! allocation of variables

   allocate (f1, source = base(4,1)(1.0,2,'A'))                                          !<- basic test  ! tcx: (4,1)
   allocate (f2(1), source = (/ base(4,1)(3.333, 101, 'Z') /) ) ! tcx: (4,1)
   allocate (base(4,1):: f3, f4(1)) ! tcx: base(4,1)

   open (unit = 1, file ='position001kl.1', form='formatted', access='sequential')


   ! formatted I/O operations

   write (1, *, iostat=stat, iomsg=msg)                f1
   if ( ( stat /= 0  ) .or. ( msg /= 'dtiowrite' ) )   error stop 1_4
   write (1, *, iostat=stat, iomsg=msg)                f2
   if ( ( stat /= 0  ) .or. ( msg /= 'dtiowrite' ) )   error stop 2_4

   rewind 1

   read (1, *, iostat=stat, iomsg=msg)                 f3
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )   error stop 3_4
   read (1, *, iostat=stat, iomsg=msg)                 f4
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )   error stop 4_4

   ! check if the values are read correctly

   if ( (.not. precision_r4(f3%r,1.0)) .or. ( f3%i /= 2 ) .or. (f3%c /= 'A' ) )  error stop 5_4
   if ( (.not. precision_r4(f4(1)%r,3.333)) .or. ( f4(1)%i /= 101 ) .or. (f4(1)%c /= 'Z' ) )  error stop 6_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1
   class(base(4,*)), intent(inout) :: dtv ! tcx: (4,*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

10 format ( I3.0,1X,A1,TR1,F5.0 )

   read (unit, 10, iostat=iostat )          dtv%i, dtv%c, dtv%r

   iomsg = 'dtioread'

end subroutine

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1
   class(base(4,*)), intent(in) :: dtv ! tcx: (4,*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(8) :: format, format1
   integer :: stat1, stat2, stat3, stat4

10 format ( T1,I4.3,TR1,A1,1X,F5.3 )

   write (unit, 10, iostat=iostat )          dtv%i, dtv%c, dtv%r

   iomsg = 'dtiowrite'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb,lb) to invoke with (4,1) / declare with (4,*) - 10 changes
