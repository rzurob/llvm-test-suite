!*  ===================================================================
!*
!*  TEST CASE NAME             : resolve003kl
!*
!*  DATE                       : 2007-08-09 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Section 9.5.3.7.3 Resolving derived-type input/output procedure references (generic binding)
!*                                    - Make both generic type bound and interface available
!*                                        - for formatted I/O with multiple extended types with arrays
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
         generic :: write(formatted) => write
         generic :: read(formatted)  => read
   end type

   type, extends(base) :: child (lchild_1) ! lchild_1=3
      integer, len :: lchild_1
      character(lchild_1) :: d = 'xxx'
      contains
         procedure, pass :: write => writec
         procedure, pass :: read => readc
   end type

   type, extends(child) :: gen3 (lgen3_1) ! lgen3_1=3
      integer, len :: lgen3_1
      character(lgen3_1) :: e = 'xxx'
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
         class(gen3(*,*,*)), intent(in) :: dtv ! tcx: (*,*,*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3,1X,A3,1X,A3)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%d, dtv%e
         iomsg = 'dtiowriteg'

      end subroutine

      subroutine readg (dtv, unit, iotype, v_list, iostat, iomsg)
         class(gen3(*,*,*)), intent(inout) :: dtv ! tcx: (*,*,*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(A3,1X,A3,1X,A3)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%d, dtv%e
         iomsg = 'dtioreadg'

      end subroutine

end module

program resolve003kl
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

   class(base(:)), allocatable  :: b1(:) ! tcx: (:)
   class(child(:,:)), pointer     :: c1(:,:) ! tcx: (:,:)
   class(gen3(:,:,:)), allocatable  :: g1(:) ! tcx: (:,:,:)

   namelist /n1/ b1, c1, g1

   integer :: stat
   character(200) :: msg

   allocate ( b1(3), source = (/ base(3)('abc'), base(3)('def'), base(3)('ghi') /) ) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   allocate ( c1(2,2), source = reshape ( source = (/ child(3,3)('abc','ABC'),child(3,3)('def','DEF'), child(3,3)('ghi','GHI'), child(3,3)('jkl','JKL') /), shape= (/2,2/) ) ) ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3,3)
   allocate ( g1(2), source = (/ gen3(3,3,3)('abc','def','ghi'), gen3(3,3,3)('jkl','mno','pqr') /) ) ! tcx: (3,3,3) ! tcx: (3,3,3)

   open ( 1, file = 'resolve003kl.1', form='formatted', access='sequential' )

   write ( 1, *, iostat = stat, iomsg = msg )            b1, c1, g1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteg' ) )     error stop 1_4

   write ( 1, * , iostat = stat, iomsg = msg )           c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )     error stop 2_4

   write ( 1, "(1X,DT)" , iostat = stat, iomsg = msg )   g1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteg' ) )     error stop 3_4

   deallocate ( b1, c1, g1 )
   allocate (base(3):: b1(3) ) ! tcx: base(3)
   allocate (child(3,3):: c1(2,2) ) ! tcx: child(3,3)
   allocate (gen3(3,3,3):: g1(2) ) ! tcx: gen3(3,3,3)

   rewind 1

   read ( 1, "(9(1X,DT))", iostat = stat, iomsg = msg )            b1, c1, g1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadg' ) )      error stop 4_4
   if ( ( b1(1)%c /='abc' ) .or. &
        ( b1(2)%c /='def' ) .or. &
        ( b1(3)%c /='ghi' ) .or. &
        ( c1(1,1)%c /= 'abc' ) .or. ( c1(1,1)%d /= 'ABC' ) .or. &
        ( c1(2,1)%c /= 'def' ) .or. ( c1(2,1)%d /= 'DEF' ) .or. &
        ( c1(1,2)%c /= 'ghi' ) .or. ( c1(1,2)%d /= 'GHI' ) .or. &
        ( c1(2,2)%c /= 'jkl' ) .or. ( c1(2,2)%d /= 'JKL' ) .or. &
        ( g1(1)%c /='abc' ) .or. ( g1(1)%d /= 'def' ) .or. ( g1(1)%e /= 'ghi' )  .or. &
        ( g1(2)%c /='jkl' ) .or. ( g1(2)%d /= 'mno' ) .or. ( g1(2)%e /= 'pqr' ) ) error stop 5_4

   deallocate ( b1 )
   allocate ( child(3,3) :: b1(4) ) ! tcx: (3,3)

   read ( 1, "(4(1X,DT))", iostat = stat, iomsg = msg )             b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )      error stop 6_4

   select type ( b1 )
      type is ( child(*,*) ) ! tcx: (*,*)
         if ( ( b1(1)%c /= 'abc' ) .or. ( b1(1)%d /= 'ABC' ) .or. &
              ( b1(2)%c /= 'def' ) .or. ( b1(2)%d /= 'DEF' ) .or. &
              ( b1(3)%c /= 'ghi' ) .or. ( b1(3)%d /= 'GHI' ) .or. &
              ( b1(4)%c /= 'jkl' ) .or. ( b1(4)%d /= 'JKL' )  )  error stop 7_4
      class default
         error stop 8_4
   end select

   deallocate ( b1 )
   allocate ( gen3(3,3,3) :: b1(2) ) ! tcx: (3,3,3)

   read ( 1, "(1X,DT)", iostat = stat, iomsg = msg )     b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadg' ) )      error stop 8_4

   select type ( b1 )
      type is ( gen3(*,*,*) ) ! tcx: (*,*,*)
         if ( ( b1(1)%c /='abc' ) .or. ( b1(1)%d /= 'def' ) .or. ( b1(1)%e /= 'ghi' )  .or. &
              ( b1(2)%c /='jkl' ) .or. ( b1(2)%d /= 'mno' ) .or. ( b1(2)%e /= 'pqr' ) )  error stop 9_4
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
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 10 changes
! type: child - added parameters (lchild_1) to invoke with (3,3) / declare with (*,*) - 9 changes
! type: gen3 - added parameters (lgen3_1) to invoke with (3,3,3) / declare with (*,*,*) - 7 changes
