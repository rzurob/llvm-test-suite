!*  ===================================================================
!*
!*  DATE                       : 2007-08-07 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Select Type Construct
!*                                    -  selector is a polymorphic scalar entity with unformatted i/o

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
         generic :: write(unformatted) => write
         generic :: read(unformatted)  => read
   end type

   type, extends(base) :: child (kchild1) ! kchild1=4
      integer, kind :: kchild1
      integer(kchild1) :: i = -999
      contains
         procedure, pass :: write => writec
         procedure, pass :: read => readc
   end type

   contains

      subroutine writeb (dtv, unit, iostat, iomsg)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtiowriteb'

      end subroutine

      subroutine readb (dtv, unit, iostat, iomsg)
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtioreadb'

      end subroutine

      subroutine writec (dtv, unit, iostat, iomsg)
         class(child(*,4)), intent(in) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
         iomsg = 'dtiowritec'

      end subroutine

      subroutine readc (dtv, unit, iostat, iomsg)
         class(child(*,4)), intent(inout) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
         iomsg = 'dtioreadc'

      end subroutine

end module

program selectType001akl
   use m

   integer :: stat
   character(200) :: msg

   class(base(:)), pointer     :: b1 ! tcx: (:)
   class(base(:)), allocatable :: b2 ! tcx: (:)
   class(child(:,4)), allocatable, target :: c1 ! tcx: (:,4)

   allocate ( b1, source = base(3) ( 'abc' ) ) ! tcx: (3)
   allocate ( b2, source = base(3) ( 'def' ) ) ! tcx: (3)
   allocate ( c1, source = child(3,4) ( 'ghi', 10001 ) ) ! tcx: (3,4)

   open ( 1, file = 'selectType001akl.1', form='unformatted', access='sequential' )

   select type ( g => b1 )
      class is ( base(*) ) ! tcx: (*)
         write ( 1, iostat = stat, iomsg = msg ) g
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 1_4
   end select

   select type ( h => b2 )
      type is ( base(*) ) ! tcx: (*)
         write ( 1, iostat = stat, iomsg = msg ) h
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 2_4
   end select

   select type ( i => c1 )
      class is ( child(*,4) ) ! tcx: (*,4)
         write ( 1, iostat = stat, iomsg = msg ) i
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 3_4
   end select

   deallocate ( b1 )
   b1 => c1
   deallocate ( b2 )
   allocate ( b2, source = child(3,4) ( 'jkl', 100002 ) ) ! tcx: (3,4)

   select type ( h => b1 )
      class default
         write ( 1, iostat = stat, iomsg = msg ) h
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 4_4
   end select

   select type ( i => b2 )
      class default
         write ( 1, iostat = stat, iomsg = msg ) i
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 5_4
   end select

   rewind 1

   deallocate ( b2, c1 )
   allocate (base(3):: b1, b2) ! tcx: base(3)
   allocate (child(3,4)::c1) ! tcx: child(3,4)

   select type ( g => b1 )
      class is ( base(*) ) ! tcx: (*)
         read ( 1, iostat = stat, iomsg = msg ) g
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) ) error stop 6_4
   end select

   if ( b1%c /= 'abc' ) error stop 7_4

   select type ( h => b2 )
      type is ( base(*) ) ! tcx: (*)
         read ( 1, iostat = stat, iomsg = msg ) h
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) ) error stop 8_4
   end select

   if ( b2%c /= 'def' ) error stop 9_4

   select type ( i => c1 )
      class is ( child(*,4) ) ! tcx: (*,4)
         read ( 1, iostat = stat, iomsg = msg ) i
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) ) error stop 10_4
   end select

   if ( ( c1%c /= 'ghi' ) .or. ( c1%i /= 10001 ) ) error stop 11_4

   c1%c = 'xxx'
   c1%i = -999

   deallocate ( b1 )
   b1 => c1
   deallocate ( b2 )
   allocate ( child(3,4) :: b2 ) ! tcx: (3,4)

   select type ( h => b1 )
      class default
         read ( 1, iostat = stat, iomsg = msg ) h
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) ) error stop 12_4
   end select

   select type ( b1 )
      type is ( child(*,4) ) ! tcx: (*,4)
         if ( ( b1%c /= 'ghi' ) .or. ( b1%i /= 10001 ) .or. ( c1%c /= 'ghi' ) .or. ( c1%i /= 10001 ) ) error stop 13_4
   end select

   select type ( i => b2 )
      class default
         read ( 1, iostat = stat, iomsg = msg ) i
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) ) error stop 14_4
   end select

   select type ( b2 )
      type is ( child(*,4) ) ! tcx: (*,4)
         if ( ( b2%c /= 'jkl' ) .or. ( b2%i /= 100002 ) ) error stop 15_4
   end select

   close ( 1, status ='delete')

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase1) to invoke with (3) / declare with (*) - 10 changes
! type: child - added parameters (kchild1) to invoke with (3,4) / declare with (*,4) - 10 changes
