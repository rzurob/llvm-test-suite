! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-07-21 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.7.3: Colon Editing
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

   type :: base (lb) ! lb=3
      integer, len :: lb
      character(lb), allocatable :: c1(:)
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

program colon001kl
   use m1

   ! declaration of variables

   class(base(:)), allocatable :: f1 ! tcx: (:)
   type(base(:)), pointer      :: f2(:) ! tcx: (:)
   type(base(:)) , allocatable :: f3 ! tcx: (:)
   class(base(:)), pointer     :: f4(:) ! tcx: (:)

   integer :: stat
   character(200) :: msg

   open ( 1, file = 'colon001kl.1', form='formatted', access='sequential' )

   ! allocation of variables

   allocate (base(3) :: f1) ! tcx: base(3)
   allocate (base(3) :: f2(2)) ! tcx: base(3)
   allocate (base(3) :: f3) ! tcx: base(3)
   allocate (base(3) :: f4(2)) ! tcx: (3)

   allocate ( f1%c1(3), source = (/'abc','def','ghi'/) )
   allocate ( f2(1)%c1(4), source = (/'ABC','DEF','GHI', 'JKL'/) )
   allocate ( f2(2)%c1(3), source = (/'MNO','PQR','STU'       /) )

   ! formatted I/O operations

   write (1, *, iostat=stat, iomsg=msg)                f1
   if ( ( stat /= 0  ) .or. ( msg /= 'dtiowrite' ) )   error stop 1_4

   write (1, *, iostat=stat, iomsg=msg)                f2
   if ( ( stat /= 0  ) .or. ( msg /= 'dtiowrite' ) )   error stop 2_4

   write (1, *, iostat=stat, iomsg=msg)         '123456789012345678901234567890'  !<- enough for 10 character(3)

   backspace 1

   read (1, *, iostat=stat, iomsg=msg)                 f3
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )    error stop 3_4

   allocate ( f4(1)%c1(2) )

   backspace 1

   read (1, *, iostat=stat, iomsg=msg)                 f4
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )    error stop 4_4


   ! check if values are read correctly

   if ( (  f3%c1(1) /= '123' ) .or. (  f3%c1(2) /= '456' )  .or.  (  f3%c1(3) /= '789' )  )              error stop 5_4
   if ( (  f4(1)%c1(1) /= '123' ) .or. (  f4(1)%c1(2) /= '456' ) .or. &
        (  f4(2)%c1(1) /= '789' ) .or. (  f4(2)%c1(2) /= '012' ) .or. (  f4(2)%c1(3) /= '345' )  )       error stop 6_4

   close (1, status='delete' )
end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1, only: base
   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( .not. allocated ( dtv%c1 ) ) allocate ( dtv%c1(3) )

10 format (999999(A,:),/)    !<- we want to maximize the number of format needed (since the size of the c1 component is variable
   read (unit, 10, iostat=iostat )      dtv%c1

   iomsg = 'dtioread'

end subroutine

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1, only: base
   class(base(*)), intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

10 format (999999(A3,:),/)    !<- we want to maximize the number of format needed (since the size of the c1 component is variable
   write (unit, 10, iostat=iostat )      dtv%c1

   iomsg = 'dtiowrite'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 9 changes