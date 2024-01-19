! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-07-23 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.7.2: Slash Editing
!*                                        Try slash editor with READ, and / editor should move position
!*                                        of the file should be placed at the beginning of the next record
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

program slash003kl
   use m1

   ! declaration of variables

   type(base(:,:)) , allocatable :: f3 ! tcx: (:,:)
   class(base(:,:)), pointer      :: f4(:) ! tcx: (:,:)

   integer :: stat
   character(200) :: msg

   open ( unit = 1, file = 'slash003kl.1', access='stream', form='formatted' )

   ! allocation of variables

   allocate (base(3,2)::f3) ! tcx: base(3,2)
   allocate (base(3,2) :: f4(2)) ! tcx: (3,2)

   ! formatted I/O operations

   write (1, *, iostat=stat, iomsg=msg)                '@ABCxxxxxxxxx'
   write (1, *, iostat=stat, iomsg=msg)                'DEFxxxxxxxxx'
   write (1, *, iostat=stat, iomsg=msg)                'GHIxxxxxxxxx'
   write (1, *, iostat=stat, iomsg=msg)                '@abcxxxxxxxxx'
   write (1, *, iostat=stat, iomsg=msg)                'defxxxxxxxxx'
   write (1, *, iostat=stat, iomsg=msg)                'ghixjklxxxxx'
   write (1, *, iostat=stat, iomsg=msg)                'mnoxxxxxxxxx'
   write (1, *, iostat=stat, iomsg=msg)                'pqrxxxxxxxxx'

   rewind 1

   read (1, *, iostat=stat, iomsg=msg)                 f3
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )    error stop 3_4
   msg = ''

   read (1, *, iostat=stat, iomsg=msg)                 f4
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )    error stop 4_4

   ! check if values are read correctly

   if ( (  f3%c1 /= 'ABC' ) .or. (  f3%c2(1) /= 'DEF' )  .or. (  f3%c2(2) /= 'GHI' )  )               error stop 5_4
   if ( (  f4(1)%c1 /= 'abc' ) .or. (  f4(1)%c2(1) /= 'def' ) .or. (  f4(1)%c2(2) /= 'ghi' ) .or. &
        (  f4(2)%c1 /= 'jkl' ) .or. (  f4(2)%c2(1) /= 'mno' ) .or. (  f4(2)%c2(2) /= 'pqr' )  )       error stop 6_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1, only: base
   class(base(*,*)), intent(inout) :: dtv ! tcx: (*,*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

10 format ( 1X,A,/,1X,A,/,1X,A )
   read (unit, 10, iostat=iostat )                      dtv%c1, dtv%c2

   iomsg = 'dtioread'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb1,lb2) to invoke with (3,2) / declare with (*,*) - 5 changes
