!*  ===================================================================
!*
!*  TEST CASE NAME             : scalar010lk
!*
!*  DATE                       : 2007-08-13 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Usage of GENERIC BINDING
!*                                  - scalar derived type with multiple level of heirarchy, and use parent component in child dtio
!*                                    with formatted I/O
!*                               adaptation: exposed kind, length; inherited length
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

   type, extends(base) :: child (kchild_1) ! kchild_1=4
      integer, kind :: kchild_1
      integer(kchild_1) :: i = -999
      contains
         procedure, pass :: write => writec
         procedure, pass :: read => readc
   end type

   type, extends(child) :: gen3
      character(lbase_1) :: s = 'xxx'
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
         class(child(*,4)), intent(in) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(DT,I4)", iostat=iostat, iomsg=iomsg) dtv%base, dtv%i

         iomsg = 'dtiowritec'

      end subroutine

      subroutine readc (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child(*,4)), intent(inout) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(DT,I4)" , iostat=iostat, iomsg=iomsg) dtv%base, dtv%i
         iomsg = 'dtioreadc'

      end subroutine

      subroutine writeg (dtv, unit, iotype, v_list, iostat, iomsg)
         class(gen3(*,4)), intent(in) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(DT,A3)", iostat=iostat, iomsg=iomsg) dtv%child, dtv%s

         iomsg = 'dtiowriteg'

      end subroutine

      subroutine readg (dtv, unit, iotype, v_list, iostat, iomsg)
         class(gen3(*,4)), intent(inout) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(DT,A3)" , iostat=iostat, iomsg=iomsg) dtv%child, dtv%s
         iomsg = 'dtioreadg'

      end subroutine

end module

program scalar010lk
   use m

   integer :: stat
   character(200) :: msg

   class(base(:)), allocatable :: b1 ! tcx: (:)
   class(child(:,4)), pointer    :: c1 ! tcx: (:,4)

   open ( 1, file = 'scalar010lk.1', form='formatted', access='sequential' )

   allocate ( b1, source = child(3,4) ( 'abc', 101 ) ) ! tcx: (3,4)
   allocate ( c1, source = gen3(3,4)  ( 'def', 102, 'DEF' ) ) ! tcx: (3,4)

   write ( 1, *, iostat = stat, iomsg = msg ) b1, c1

   deallocate ( b1 )
   allocate ( b1, source = gen3(3,4) ( 'ghi', 103, 'GHI' ) ) ! tcx: (3,4)

   write ( 1, "(DT)", iostat = stat, iomsg = msg ) b1

   rewind 1

   deallocate ( b1, c1 )
   allocate ( child(3,4) :: b1 ) ! tcx: (3,4)
   allocate ( gen3(3,4) :: c1 ) ! tcx: (3,4)

   read ( 1, "(1x,DT,1x,dt)", iostat = stat, iomsg = msg ) b1, c1

   select type ( b1 )
      type is ( child(*,4) ) ! tcx: (*,4)

        if ( ( b1%c /= 'abc' ) .or. ( b1%i /= 101 ) ) error stop 101_4
   end select

   select type ( c1 )
      type is ( gen3(*,4) ) ! tcx: (*,4)
        if ( ( c1%c /= 'def' ) .or. ( c1%i /= 102 ) .or. ( c1%s /= 'DEF' ) ) error stop 2_4
   end select

   deallocate ( b1 )
   allocate ( b1, source = gen3(3,4) ( ) ) ! tcx: (3,4)

   read ( 1, "(DT)", iostat = stat, iomsg = msg ) b1

   select type ( b1 )
      type is ( gen3(*,4) ) ! tcx: (*,4)
        if ( ( b1%c /= 'ghi' ) .or. ( b1%i /= 103 ) .or. ( b1%s /= 'GHI' ) ) error stop 3_4
   end select

   close ( 1, status ='delete')

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 3 changes
! type: child - added parameters (kchild_1) to invoke with (3,4) / declare with (*,4) - 6 changes
! type: gen3 - added parameters () to invoke with (3,4) / declare with (*,4) - 8 changes
