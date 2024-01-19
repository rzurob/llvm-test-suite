! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-09-10 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Use Association with rename
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
   type base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      character(lbase_1) :: c
   end type

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base(:)), allocatable :: b1 ! tcx: (:)
   class(base(:)), pointer     :: b2 ! tcx: (:)

end module


program rename001l
   use m1, newbase => base

   ! declaration of variables

   type(newbase(:)) , allocatable :: b3 ! tcx: (:)
   class(newbase(:)), pointer     :: b4(:) ! tcx: (:)
   character(3) :: c1, c2, c3
   character(9) :: c4

   integer :: stat
   character(200) :: msg

   open (unit = 3, file ='rename001l.3', form='unformatted', access='stream')

   ! allocation of variables

   allocate (b1, source = newbase(3)('abc') ) ! tcx: (3)
   allocate (b2, source = newbase(3)('def') ) ! tcx: (3)
   allocate (b3, source = newbase(3)('ghi') ) ! tcx: (3)
   allocate (b4(3), source = (/ b3, b2, b1 /) )

   ! unformatted I/O operations

   write ( 3, iostat = stat, iomsg = msg, pos=1 )               b1
      if ( (stat /= 0 ) .or. ( msg /= 'dtiowrite') )            error stop 1_4

   select type ( b2 )
      type is ( newbase(*) ) ! tcx: (*)
         write ( 3, iostat = stat, iomsg = msg, pos=4 )         b2
            if ( (stat /= 0 ) .or. ( msg /= 'dtiowrite') )      error stop 2_4
      class default
         error stop 3_4
   end select

   write ( 3, iostat = stat, iomsg = msg, pos=7 )         b3
   if ( (stat /= 0 ) .or. ( msg /= 'dtiowrite') )      error stop 3_4

   associate ( b14 => b4 )
      select type ( b14 )
        class is (newbase(*)) ! tcx: (*)
            write ( 3, iostat = stat, iomsg = msg, pos=10 )     b14
               if ( (stat /= 0 ) .or. ( msg /= 'dtiowrite') )   error stop 4_4
      end select
   end associate

   rewind 3

   read (3, iostat = stat, iomsg = msg, pos = 1)     c1
   read (3, iostat = stat, iomsg = msg, pos = 4)     c2
   read (3, iostat = stat, iomsg = msg, pos = 7)     c3
   read (3, iostat = stat, iomsg = msg, pos = 10)    c4

   if ( c1 /= 'abc' )       error stop 5_4
   if ( c2 /= 'def' )       error stop 6_4
   if ( c3 /= 'ghi' )       error stop 7_4
   if ( c4 /= 'ghidefabc' ) error stop 8_4

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base
   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read (unit,  iostat=iostat ) dtv%c

   iomsg = 'dtioread'

end subroutine

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1,  only: base
   class(base(*)), intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write (unit, iostat=iostat ) dtv%c

   iomsg = 'dtiowrite'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 6 changes
! type: newbase - invoke with (3) / declare with (*) - 7 changes
