!*  ===================================================================
!*
!*  DATE                       : 2007-08-09 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Section 9.5.3.7.3 Resolving derived-type input/output procedure references (generic binding)
!*                                    - Make both generic type bound and interface available
!*                                        - for formatted I/O with multiple extended types
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

   type base (lbase1) ! lbase1=3
      integer, len :: lbase1
      character(lbase1) :: c = 'xxx'
      contains
         procedure, pass :: write => writeb
         procedure, pass :: read => readb
         generic :: write(formatted) => write
         generic :: read(formatted)  => read
   end type

   type, extends(base) :: child (lchild1) ! lchild1=3
      integer, len :: lchild1
      character(lchild1) :: d = 'xxx'
      contains
         procedure, pass :: write => writec
         procedure, pass :: read => readc
   end type

   type, extends(child) :: gen3
      character(lbase1) :: e = 'xxx'
      contains
         procedure, pass :: write => writeg
         procedure, pass :: read => readg
   end type

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

      subroutine writeg (dtv, unit, iotype, v_list, iostat, iomsg)
         class(gen3(*,*)), intent(in) :: dtv ! tcx: (*,*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3,1X,A3,1X,A3)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%d, dtv%e
         iomsg = 'dtiowriteg'

      end subroutine

      subroutine readg (dtv, unit, iotype, v_list, iostat, iomsg)
         class(gen3(*,*)), intent(inout) :: dtv ! tcx: (*,*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(A3,1X,A3,1X,A3)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%d, dtv%e
         iomsg = 'dtioreadg'

      end subroutine

end module

program resolve002ll
   use m

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

   class(base(:)), allocatable  :: b1 ! tcx: (:)
   class(child(:,:)), pointer     :: c1 ! tcx: (:,:)
   class(gen3(:,:)), allocatable  :: g1 ! tcx: (:,:)

   namelist /n1/ b1, c1, g1

   integer :: stat
   character(200) :: msg

   allocate ( b1, source = base(3)('abc') ) ! tcx: (3)
   allocate ( c1, source = child(3,3)('def','ghi') ) ! tcx: (3,3)
   allocate ( g1, source = gen3(3,3)('mno','pqr','stu') ) ! tcx: (3,3)

   open ( 1, file = 'resolve002ll.1', form='formatted', access='sequential' )

   write ( 1, n1, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteg' ) )     error stop 1_4

   write ( 1, * , iostat = stat, iomsg = msg )           c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )     error stop 2_4

   write ( 1, "(1X,DT)" , iostat = stat, iomsg = msg )   g1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteg' ) )     error stop 3_4

   deallocate ( b1, c1, g1 )
   allocate (base(3):: b1 ) ! tcx: base(3)
   allocate (child(3,3):: c1 ) ! tcx: child(3,3)
   allocate (gen3(3,3):: g1 ) ! tcx: gen3(3,3)
   rewind 1

   read ( 1, n1, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadg' ) )      error stop 4_4
   if ( ( b1%c /='abc' ) .or. ( c1%c /= 'def' ) .or. ( c1%d /= 'ghi' ) .or. &
        ( g1%c /='mno' ) .or. ( g1%d /= 'pqr' ) .or. ( g1%e /= 'stu' )  ) error stop 5_4

   deallocate ( b1 )
   allocate ( b1, source = child(3,3)() ) ! tcx: (3,3)

   read ( 1, "(1X,DT)", iostat = stat, iomsg = msg )             b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )      error stop 6_4

   select type ( b1 )
      type is ( child(*,*) ) ! tcx: (*,*)
         if ( ( b1%c /='def' ) .or. ( b1%d /= 'ghi' ) )  error stop 7_4
      class default
         error stop 8_4
   end select

   deallocate ( b1 )
   allocate ( b1, source = gen3(3,3)() ) ! tcx: (3,3)

   read ( 1, "(1x,DT)", iostat = stat, iomsg = msg )     b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadg' ) )      error stop 8_4

   select type ( b1 )
      type is ( gen3(*,*) ) ! tcx: (*,*)
         if ( ( b1%c /='mno' ) .or. ( b1%d /= 'pqr' ) .or. ( b1%e /= 'stu' ) )  error stop 9_4
      class default
         error stop 10_4
   end select

   close ( 1, status ='delete')

end program

subroutine writebaseext (dtv, unit, iotype, v_list, iostat, iomsg)
   use m
   class(base(*)), intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)  :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   error stop 11_4
   iomsg = 'ERROR'

end subroutine

subroutine readbaseext (dtv, unit, iotype, v_list, iostat, iomsg)
   use m
   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)  :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   error stop 12_4
   iomsg = 'ERROR'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase1) to invoke with (3) / declare with (*) - 8 changes
! type: child - added parameters (lchild1) to invoke with (3,3) / declare with (*,*) - 6 changes
! type: gen3 - added parameters () to invoke with (3,3) / declare with (*,*) - 6 changes
