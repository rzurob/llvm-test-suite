!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : resolve004kl
!*
!*  PROGRAMMER                 : David Forster (derived from resolve004 by Robert Ma)
!*  DATE                       : 2007-08-09 (original: 04/26/2005)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Section 9.5.3.7.3 Resolving derived-type input/output procedure references (generic binding)
!*                                    - Generic type bound is private and interface is public
!*                                        - for formatted I/O, inside module, generic tb should be called
!*                                          outside module, interface dtio shall be called
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      character(lbase_1) :: c = 'xxx'
      contains
         procedure, pass :: write => writeb
         procedure, pass :: read => readb
         generic, private :: write(formatted) => write
         generic, private :: read(formatted)  => read
   end type

   type, extends(base) :: child (lchild_1) ! lchild_1=3
      integer, len :: lchild_1
      character(lchild_1) :: d = 'xxx'
      contains
         procedure, pass :: write => writec
         procedure, pass :: read => readc
   end type

   interface write(formatted)
      subroutine writebaseext (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   interface read(formatted)
      subroutine readbaseext (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   contains

      subroutine writeb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3)", iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtiowriteb'

      end subroutine

      subroutine readb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(A3)" , iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtioreadb'

      end subroutine

      subroutine writec (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child(*,*)), intent(in) :: dtv ! tcx: (*,*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3,1X,A3)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%d
         iomsg = 'dtiowritec'

      end subroutine

      subroutine readc (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child(*,*)), intent(inout) :: dtv ! tcx: (*,*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(A3,1X,A3)" , iostat=iostat, iomsg=iomsg) dtv%c, dtv%d
         iomsg = 'dtioreadc'

      end subroutine

      subroutine myread( dtv, iostat, iomsg )
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read ( 1, "(1X,DT)", iostat = iostat, iomsg = iomsg ) dtv

      end subroutine

      subroutine mywrite( dtv, iostat, iomsg )
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write ( 1, *, iostat = iostat, iomsg = iomsg ) dtv

      end subroutine

end module

program resolve004kl
   use m

   class(base(:)), allocatable  :: b1 ! tcx: (:)
   class(child(:,:)), pointer     :: c1 ! tcx: (:,:)

   integer :: stat
   character(200) :: msg

   allocate ( b1, source = base(3)('abc') ) ! tcx: (3)
   allocate ( c1, source = child(3,3)('def','ghi') ) ! tcx: (3,3)

   open ( 1, file = 'resolve004kl.1', form='formatted', access='sequential' )

   call mywrite ( b1, stat, msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 1_4

   call mywrite ( c1, stat, msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 2_4

   deallocate ( b1 )
   allocate ( b1, source = child(3,3)('jkl','mno') ) ! tcx: (3,3)

   call mywrite ( b1, stat, msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 3_4

   deallocate ( b1 )
   allocate ( b1, source = base(3)('abc') ) ! tcx: (3)

   write ( 1, * , iostat = stat, iomsg = msg )   b1
   if ( ( stat /= 0 ) .or. ( msg /= 'extdtiowrite' ) ) error stop 4_4

   write ( 1, * , iostat = stat, iomsg = msg )   c1
   if ( ( stat /= 0 ) .or. ( msg /= 'extdtiowrite' ) ) error stop 5_4

   deallocate ( b1 )
   allocate ( b1, source = child(3,3)('jkl','mno') ) ! tcx: (3,3)

   write ( 1, * , iostat = stat, iomsg = msg )   b1
   if ( ( stat /= 0 ) .or. ( msg /= 'extdtiowrite' ) ) error stop 6_4

   rewind 1

   deallocate ( b1, c1 )
   allocate (base(3):: b1 ) ! tcx: base(3)
   allocate (child(3,3):: c1 ) ! tcx: child(3,3)

   call myread ( b1, stat, msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) ) error stop 7_4
   if ( b1%c /= 'abc' )                             error stop 8_4

   call myread ( c1, stat, msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) ) error stop 9_4
   if ( ( c1%c /= 'def' ) .or. ( c1%d /= 'ghi' ) )  error stop 10_4

   deallocate ( b1 )
   allocate ( child(3,3) :: b1 ) ! tcx: (3,3)

   call myread ( b1, stat, msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) ) error stop 11_4
   select type ( b1 )
      type is ( child(*,*) ) ! tcx: (*,*)
         if ( ( b1%c /= 'jkl' ) .or. ( b1%d /= 'mno' ) )  error stop 12_4
      class default
         error stop 13_4
   end select

   deallocate ( b1, c1 )
   allocate (base(3):: b1 ) ! tcx: base(3)
   allocate (child(3,3):: c1 ) ! tcx: child(3,3)

   read ( 1, "(1X,DT)" , iostat = stat, iomsg = msg )   b1
   if ( ( stat /= 0 ) .or. ( msg /= 'extdtioread' ) ) error stop 14_4
   if ( b1%c /= 'abc' )                               error stop 15_4

   read ( 1, "(1X,DT)" , iostat = stat, iomsg = msg )   c1
   if ( ( stat /= 0 ) .or. ( msg /= 'extdtioread' ) ) error stop 16_4
   if ( ( c1%c /= 'def' ) .or. ( c1%d /= 'ghi' ) )    error stop 17_4

   deallocate ( b1 )
   allocate ( b1, source = child(3,3)('jkl','mno') ) ! tcx: (3,3)

   read ( 1, "(1X,DT)" , iostat = stat, iomsg = msg )   b1
   if ( ( stat /= 0 ) .or. ( msg /= 'extdtioread' ) ) error stop 18_4

   select type ( b1 )
      type is ( child(*,*) ) ! tcx: (*,*)
         if ( ( b1%c /= 'jkl' ) .or. ( b1%d /= 'mno' ) )  error stop 19_4
      class default
         error stop 20_4
   end select

   close ( 1, status ='delete')

end program

subroutine writebaseext (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child
  class(base(*)), intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)  :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type ( dtv )
      type is (base(*)) ! tcx: (*)
         write (unit, "(A3)", iostat=iostat, iomsg=iomsg)       dtv%c
      type is (child(*,*)) ! tcx: (*,*)
         write (unit, "(A3,1X,A3)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%d
   end select

   iomsg = 'extdtiowrite'

end subroutine

subroutine readbaseext (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child
   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)  :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type ( dtv )
      type is (base(*)) ! tcx: (*)
         read (unit, "(A3)", iostat=iostat, iomsg=iomsg)       dtv%c
      type is (child(*,*)) ! tcx: (*,*)
         read (unit, "(A3,1X,A3)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%d
   end select

   iomsg = 'extdtioread'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 13 changes
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 13 changes
! type: child - added parameters (lchild_1) to invoke with (3,3) / declare with (*,*) - 12 changes
