!*  ===================================================================
!*
!*  DATE                       : 2007-08-09 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Section 9.5.3.7.3 Resolving derived-type input/output procedure references (generic binding)
!*                                    - Generic type bound is available in extended types, and interface available for base type
!*                                        - for formatted I/O, Outside select type construct -> interface used
!*                                                             Inside select type construct -> generic used
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
   end type

   type, extends(base) :: child (lchild_1) ! lchild_1=3
      integer, len :: lchild_1
      character(lchild_1) :: d = 'xxx'
      contains
         procedure, pass :: write => writec
         procedure, pass :: read => readc
         generic :: write(formatted) => write
         generic :: read(formatted)  => read
   end type

   type, extends(child) :: gen3 (lgen3_1) ! lgen3_1=3
      integer, len :: lgen3_1
      character(lgen3_1) :: e = 'xxx'
      contains
         procedure, pass :: write => writeg
         procedure, pass :: read => readg
   end type

   contains

      subroutine writec (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child(*,*)), intent(in) :: dtv ! tcx: (*,*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3,1X,A3)", iostat=iostat, iomsg=iomsg)    dtv%c, dtv%d
         iomsg = 'dtiowritec'

      end subroutine

      subroutine readc (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child(*,*)), intent(inout) :: dtv ! tcx: (*,*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(A3,1X,A3)", iostat=iostat, iomsg=iomsg)    dtv%c, dtv%d
         iomsg = 'dtioreadc'

      end subroutine

      subroutine writeg (dtv, unit, iotype, v_list, iostat, iomsg)
         class(gen3(*,*,*)), intent(in) :: dtv ! tcx: (*,*,*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3,1X,A3,1X,A3)", iostat=iostat, iomsg=iomsg)    dtv%c, dtv%d, dtv%e
         iomsg = 'dtiowriteg'

      end subroutine

      subroutine readg (dtv, unit, iotype, v_list, iostat, iomsg)
         class(gen3(*,*,*)), intent(inout) :: dtv ! tcx: (*,*,*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(A3,1X,A3,1X,A3)", iostat=iostat, iomsg=iomsg)    dtv%c, dtv%d, dtv%e
         iomsg = 'dtioreadg'

      end subroutine

end module

program resolve007lll
   use m

   interface write(formatted)
      subroutine writebaseext (dtv, unit, iotype, v_list, iostat, iomsg)
         use m, only: base, child
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface write(formatted)

   interface read(formatted)
      subroutine readbaseext (dtv, unit, iotype, v_list, iostat, iomsg)
         use m, only: base, child
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface read(formatted)

   class(base(:)), allocatable  :: b1 ! tcx: (:)
   class(base(:)), pointer      :: b2 ! tcx: (:)

   integer :: stat
   character(200) :: msg

   allocate ( b1, source = base(3)('abc') ) ! tcx: (3)

   open ( 1, file = 'resolve007lll.1', form='formatted', access='sequential' )

   write ( 1, "(DT)", iostat = stat, iomsg = msg )      b1
   if ( ( stat /= 0 ) .or. ( msg /= 'extdtiowriteb' ) ) error stop 1_4

   deallocate ( b1 )
   allocate ( b1, source = child(3,3)('def','ghi') ) ! tcx: (3,3)

   write ( 1, "(DT)", iostat = stat, iomsg = msg )      b1
   if ( ( stat /= 0 ) .or. ( msg /= 'extdtiowritec' ) ) error stop 2_4

   select type ( b1 )
      class is ( child(*,*) ) ! tcx: (*,*)
         write ( 1, "(DT)", iostat = stat, iomsg = msg )      b1
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )    error stop 3_4
   end select

   deallocate ( b1 )
   allocate ( b1, source = gen3(3,3,3)('jkl','mno','pqr') ) ! tcx: (3,3,3)

   write ( 1, "(DT)", iostat = stat, iomsg = msg )      b1
   if ( ( stat /= 0 ) .or. ( msg /= 'extdtiowriteg' ) ) error stop 4_4

   select type ( b1 )
      type is ( gen3(*,*,*) ) ! tcx: (*,*,*)
         write ( 1, "(DT)", iostat = stat, iomsg = msg )      b1
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteg' ) )    error stop 5_4
   end select

   rewind 1

   allocate (base(3):: b2 ) ! tcx:  base(3)

   read ( 1, "(DT)", iostat = stat, iomsg = msg )      b2
   if ( ( stat /= 0 ) .or. ( msg /= 'extdtioreadb' ) ) error stop 6_4

   if ( b2%c /= 'abc' ) error stop 7_4

   deallocate ( b2 )
   allocate ( child(3,3) :: b2 ) ! tcx: (3,3)

   read ( 1, "(DT)", iostat = stat, iomsg = msg )      b2
   if ( ( stat /= 0 ) .or. ( msg /= 'extdtioreadc' ) ) error stop 7_4

   select type ( b2 )
      class is ( child(*,*) ) ! tcx: (*,*)

         if ( ( b2%c /= 'def' ) .or. ( b2%d /= 'ghi' ) ) error stop 8_4
         b2%c = 'xxx'
         b2%d = 'xxx'

         read( 1, "(DT)", iostat = stat, iomsg = msg )      b2
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )    error stop 9_4

         if ( ( b2%c /= 'def' ) .or. ( b2%d /= 'ghi' ) ) error stop 10_4

   end select

   deallocate ( b2 )
   allocate ( b2, source = gen3(3,3,3)('jkl','mno','pqr') ) ! tcx: (3,3,3)

   read ( 1, "(DT)", iostat = stat, iomsg = msg )      b2
   if ( ( stat /= 0 ) .or. ( msg /= 'extdtioreadg' ) ) error stop 11_4

   select type ( b2 )
      type is ( gen3(*,*,*) ) ! tcx: (*,*,*)

         if ( ( b2%c /= 'jkl' ) .or. ( b2%d /= 'mno' ) .or. ( b2%e /= 'pqr' ) ) error stop 12_4
         b2%c = 'xxx'
         b2%d = 'xxx'
         b2%e = 'xxx'

         read ( 1, "(DT)", iostat = stat, iomsg = msg )      b2
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadg' ) )    error stop 13_4
         if ( ( b2%c /= 'jkl' ) .or. ( b2%d /= 'mno' ) .or. ( b2%e /= 'pqr' ) ) error stop 14_4
   end select

   close ( 1, status ='delete')

end program

subroutine writebaseext (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child, gen3
   class(base(*)), intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)  :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   selecttype ( dtv )
      type is (base(*)) ! tcx: (*)
         write (unit, "(A3)", iostat=iostat, iomsg=iomsg)             dtv%c
         iomsg = 'extdtiowriteb'
      type is (child(*,*)) ! tcx: (*,*)
         write (unit, "(A3,1X,A3)", iostat=iostat, iomsg=iomsg)       dtv%c, dtv%d
         iomsg = 'extdtiowritec'
      type is (gen3(*,*,*)) ! tcx: (*,*,*)
         write (unit, "(A3,1X,A3,1X,A3)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%d, dtv%e
         iomsg = 'extdtiowriteg'
   end select

end subroutine

subroutine readbaseext (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child, gen3
   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)  :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type ( dtv )
      type is (base(*)) ! tcx: (*)
         read (unit, "(A3)", iostat=iostat, iomsg=iomsg)             dtv%c
         iomsg = 'extdtioreadb'
      type is (child(*,*)) ! tcx: (*,*)
         read (unit, "(A3,1X,A3)", iostat=iostat, iomsg=iomsg)       dtv%c, dtv%d
         iomsg = 'extdtioreadc'
      type is (gen3(*,*,*)) ! tcx: (*,*,*)
         read (unit, "(A3,1X,A3,1X,A3)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%d, dtv%e
         iomsg = 'extdtioreadg'
   end select

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 9 changes
! type: child - added parameters (lchild_1) to invoke with (3,3) / declare with (*,*) - 8 changes
! type: gen3 - added parameters (lgen3_1) to invoke with (3,3,3) / declare with (*,*,*) - 8 changes
