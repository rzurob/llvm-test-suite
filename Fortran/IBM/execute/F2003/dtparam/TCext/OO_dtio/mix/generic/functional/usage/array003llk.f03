!*  ===================================================================
!*
!*  DATE                       : 2007-08-09 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Usage of GENERIC BINDING
!*                                  - array derived type variable containing polymorphic
!*                                    components which has DTIO with formatted I/O
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

   type :: container (kcontainer_1) ! kcontainer_1=4
      integer, kind :: kcontainer_1
      integer(kcontainer_1) :: i
      class(base(:)), allocatable :: b ! tcx: (:)
      contains
         procedure, pass :: write => writecon
         procedure, pass :: read => readcon
         generic :: write(formatted) => write
         generic :: read(formatted)  => read
   end type

   contains

      subroutine writecon (dtv, unit, iotype, v_list, iostat, iomsg)
         class(container(4)), intent(in) :: dtv ! tcx: (4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(I4,DT)", iostat=iostat, iomsg=iomsg) dtv%i, dtv%b

         select type ( g => dtv%b )
            type is ( base(*) ) ! tcx: (*)
               if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtiowriteb' ) ) error stop 101_4
            type is ( child(*,*) ) ! tcx: (*,*)
               if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtiowritec' ) ) error stop 2_4
         end select

         iomsg = 'dtiowritecon'

      end subroutine

      subroutine readcon (dtv, unit, iotype, v_list, iostat, iomsg)
         class(container(4)), intent(inout) :: dtv ! tcx: (4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(I4,DT)", iostat=iostat, iomsg=iomsg) dtv%i, dtv%b

         select type ( g => dtv%b )
            type is ( base(*) ) ! tcx: (*)
               if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtioreadb' ) ) error stop 3_4
            type is ( child(*,*) ) ! tcx: (*,*)
               if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtioreadc' ) ) error stop 4_4
         end select

         iomsg = 'dtioreadcon'

      end subroutine

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

end module

program array003llk
   use m

   class(container(4)), allocatable :: b1(:) ! tcx: (4)
   type(container(4)) :: b2(2,2) ! tcx: (4)

   integer :: stat
   character(200) :: msg

   open ( 1, file = 'array003llk.1', form='formatted', access='sequential' )

   allocate ( b1(3), source = (/ container(4)( 101, base(3)('abc') ), container(4)( 102, base(3)('def') ), container(4)( 103, base(3)('ghi') ) /) ) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (4) ! tcx: (4) ! tcx: (4)
   b2  = reshape( source = (/ container(4)(201, child(3,3)('ABC','abc')), container(4)(202, child(3,3)('DEF','def')), container(4)(203, child(3,3)('GHI','ghi')), container(4)(204, child(3,3)('JKL','jkl')) /), & ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3,3) ! tcx: (4) ! tcx: (4) ! tcx: (4) ! tcx: (4)
                                         shape = (/ 2,2 /) )
   write ( 1, *, iostat = stat, iomsg = msg )             b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritecon' ) )    error stop 5_4

   write ( 1, "(DT)", iostat = stat, iomsg = msg )        b2             !<- reversion occurs
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritecon' ) )    error stop 6_4

   rewind 1

   deallocate ( b1 )
   allocate (container(4):: b1(3) ) ! tcx: container(4)
   allocate ( base(3) :: b1(1)%b, b1(2)%b, b1(3)%b ) ! tcx: (3)

   b2 = container(4)(-999,child(3,3)()) ! tcx: (3,3) ! tcx: (4)

   read ( 1, "(3(1x,dt))", iostat = stat, iomsg = msg )                b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadcon' ) )       error stop 7_4

   read ( 1, "(DT)", iostat = stat, iomsg = msg )           b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadcon' ) )       error stop 8_4

   if ( ( b1(1)%i /= 101 ) .or. ( b1(1)%b%c /= 'abc' ) .or. &
        ( b1(2)%i /= 102 ) .or. ( b1(2)%b%c /= 'def' ) .or. &
        ( b1(3)%i /= 103 ) .or. ( b1(3)%b%c /= 'ghi' ) )    error stop 9_4

   select type ( g => b2(1,1)%b )
      type is ( child(*,*) ) ! tcx: (*,*)
         if ( ( b2(1,1)%i /= 201 ) .or. ( g%c /= 'ABC' ) .or. ( g%d /= 'abc' ) )   error stop 10_4
   end select

   select type ( g => b2(2,1)%b )
      type is ( child(*,*) ) ! tcx: (*,*)
         if ( ( b2(2,1)%i /= 202 ) .or. ( g%c /= 'DEF' ) .or. ( g%d /= 'def' ) )   error stop 11_4
   end select

   select type ( g => b2(1,2)%b )
      type is ( child(*,*) ) ! tcx: (*,*)
         if ( ( b2(1,2)%i /= 203 ) .or. ( g%c /= 'GHI' ) .or. ( g%d /= 'ghi' ) )   error stop 12_4
   end select

   select type ( g => b2(2,2)%b )
      type is ( child(*,*) ) ! tcx: (*,*)
         if ( ( b2(2,2)%i /= 204 ) .or. ( g%c /= 'JKL' ) .or. ( g%d /= 'jkl' ) )   error stop 13_4
   end select

   close ( 1, status ='delete')

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 9 changes
! type: child - added parameters (lchild_1) to invoke with (3,3) / declare with (*,*) - 13 changes
! type: container - added parameters (kcontainer_1) to invoke with (4) / declare with (4) - 12 changes