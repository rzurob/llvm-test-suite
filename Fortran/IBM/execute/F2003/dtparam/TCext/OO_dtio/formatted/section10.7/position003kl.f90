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
!*                                        If characters are transmitted to positions at of
!*                                        after the position specified by a T, TL, TR, or X,
!*                                        positions skipped and not previously filled are filled with blanks
!*                                        position editors will not overwrite any characters, but can be used to position so that
!*                                        the desired data is overwritten
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
      character(lb)  :: c1
      character(lb)  :: c2
      character(lb)  :: c3
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

program position003kl
   use m1

   ! declaration of variables

   class(base(:)), allocatable :: f1 ! tcx: (:)
   type(base(:)), pointer      :: f2(:) ! tcx: (:)
   type(base(:)) , allocatable :: f3 ! tcx: (:)
   class(base(:)) , pointer    :: f4(:) ! tcx: (:)

   integer :: stat
   character(200) :: msg

   character(30) :: internalFile(5:10)

   ! allocation of variables

   allocate (f1, source = base(3)('abc','def','ghi')) ! tcx: (3)
   allocate (f2(2), source = (/ base(3)('ABC','DEF','GHI'), base(3)('JKL','MNO','PQR') /) ) ! tcx: (3) ! tcx: (3)
   allocate (base(3)::f3) ! tcx: base(3)
   allocate (base(3) :: f4(2)) ! tcx: (3)

   ! formatted I/O operations

   write (internalFile(5), *, iostat=stat, iomsg=msg)                f1
   if ( ( stat /= 0  ) .or. ( msg /= 'dtiowrite' ) )   error stop 1_4
   write (internalFile(10), *, iostat=stat, iomsg=msg)               f2
   if ( ( stat /= 0  ) .or. ( msg /= 'dtiowrite' ) )   error stop 2_4

   read (internalFile(5), *, iostat=stat, iomsg = msg)               f3
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )    error stop 3_4
   read (internalFile(10), *, iostat=stat, iomsg = msg)              f4
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )    error stop 4_4

   ! check if values are read correctly

   if ( (  f3%c1 /= 'abc' ) .or. (  f3%c2 /= 'def' ) .or. (  f3%c3 /= 'ghi' ) )   error stop 5_4
   if ( (  f4(1)%c1 /= 'ABC' ) .or. (  f4(1)%c2 /= 'DEF' ) .or. (  f4(1)%c3 /= 'GHI' ) .or. &
        (  f4(2)%c1 /= 'JKL' ) .or. (  f4(2)%c2 /= 'MNO' ) .or. (  f4(2)%c3 /= 'PQR' ) )   error stop 6_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1, only: base
   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

10 format ( 3X,A3 )

   read (unit, 10, iostat=iostat )                     dtv%c2
   if (iostat /= 0 )   error stop 7_4
   read (unit, "(A3)", iostat = iostat )               dtv%c3
   if (iostat /= 0 )   error stop 8_4
   read (unit, "(TL9,A3,6X)", iostat = iostat )      dtv%c1

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

10 format ( 3X,A3,A3 )
   write (unit, 10, iostat=iostat )                         dtv%c2, "jnk"
   !if (iostat /= 0 )   error stop 9_4
   write (unit, "(TL10,A3,3X,A3)", iostat = iostat )     dtv%c1, dtv%c3  !<- overwriting junk, and overwriting non-written area, so 2X shall create 1 space

   iomsg = 'dtiowrite'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 12 changes
