!*  ===================================================================
!*
!*  TEST CASE NAME             : scalar003llk
!*
!*  DATE                       : 2007-08-13 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Usage of GENERIC BINDING
!*                                  - scalar (non-) polymorphic derived type entity with DTIO
!*                                    containing components which has DTIO procedure with formatted I/O
!*                               adaptation: exposed kind, lengths
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
               if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtiowritec' ) ) error stop 102_4
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
               if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtioreadb' ) ) error stop 103_4
            type is ( child(*,*) ) ! tcx: (*,*)
               if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtioreadc' ) ) error stop 104_4
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

program scalar003llk
   use m

   class(container(4)), allocatable :: c1 ! tcx: (4)
   type(container(4))               :: c2 ! tcx: (4)

   integer :: stat
   character(200) :: msg

   allocate ( c1, source = container(4)(101, base(3)('abc') ) ) ! tcx: (3) ! tcx: (4)
   c2 = container(4)(102, base(3)('def') ) ! tcx: (3) ! tcx: (4)

   open ( 1, file = 'scalar003llk.1', form='formatted', access='sequential' )

   write ( 1, "(DT,DT)", iostat = stat, iomsg = msg )    c1, c2

   deallocate ( c1, c2%b )

   allocate ( c1, source = container(4)(103, child(3,3)('ABC','DEF') ) ) ! tcx: (3,3) ! tcx: (4)
   c2 = container(4)(104, child(3,3)('GHI','JKL') ) ! tcx: (3,3) ! tcx: (4)

   write ( 1, *, iostat = stat, iomsg = msg )            c1, c2

   rewind 1

   deallocate ( c1, c2%b )

   allocate ( c1, source = container(4)(-999, base(3)('xxx') ) ) ! tcx: (3) ! tcx: (4)
   c2 = container(4)(-999, base(3)('xxx') ) ! tcx: (3) ! tcx: (4)

   read  ( 1, "(DT,DT)", iostat = stat, iomsg = msg )    c2, c1

   if ( ( c1%i /= 102 ) .or. ( c1%b%c /= 'def' ) .or. &
        ( c2%i /= 101 ) .or. ( c2%b%c /= 'abc' )      ) error stop 5_4

   deallocate ( c1, c2%b )
   allocate ( c1, source = container(4)(-999, child(3,3)('xxx','xxx') ) ) ! tcx: (3,3) ! tcx: (4)
   c2 = container(4)(-999, child(3,3)('xxx','xxx') ) ! tcx: (3,3) ! tcx: (4)

   read  ( 1, "(1x,DT,1x,DT)", iostat = stat, iomsg = msg )    c2, c1

   select type ( g => c1%b )
      type is ( child(*,*) ) ! tcx: (*,*)
         select type ( h => c2%b )
            type is ( child(*,*) ) ! tcx: (*,*)
               if ( ( c1%i /= 104 ) .or. ( g%c /= 'GHI' ) .or. ( g%d /= 'JKL' ) .or. &
                    ( c2%i /= 103 ) .or. ( h%c /= 'ABC' ) .or. ( h%d /= 'DEF' )      ) error stop 6_4
         end select
   end select

   close ( 1, status ='delete')

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 9 changes
! type: child - added parameters (lchild_1) to invoke with (3,3) / declare with (*,*) - 10 changes
! type: container - added parameters (kcontainer_1) to invoke with (4) / declare with (4) - 12 changes
