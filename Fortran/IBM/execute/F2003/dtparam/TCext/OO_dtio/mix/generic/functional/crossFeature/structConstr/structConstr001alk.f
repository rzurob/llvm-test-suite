!*  ===================================================================
!*
!*  DATE                       : 2007-08-08 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Structure Constructor
!*                                    -  Structure constructor with unformatted i/o
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
         generic :: write(unformatted) => write
   end type

   type, extends(base) :: child (kchild1) ! kchild1=4
      integer, kind :: kchild1
      integer(kchild1) :: i = -999
      contains
         procedure, pass :: write => writec
   end type

   type, extends(child) :: gen3
      character(lbase1) :: s = 'xxx'
      contains
         procedure, pass :: write => writeg
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

      subroutine writec (dtv, unit, iostat, iomsg)
         class(child(*,4)), intent(in) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i

         iomsg = 'dtiowritec'

      end subroutine

      subroutine writeg (dtv, unit, iostat, iomsg)
         class(gen3(*,4)), intent(in) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i, dtv%s

         iomsg = 'dtiowriteg'

      end subroutine

end module

program structConstr001alk
   use m

   interface read(unformatted)
      subroutine read (dtv, unit, iostat, iomsg)
         import base
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface read(unformatted)


   integer :: stat
   character(200) :: msg

   type(base(3))  :: b1 ! tcx: (3)
   type(child(3,4)) :: c1 ! tcx: (3,4)
   type(gen3(3,4))  :: g1 ! tcx: (3,4)

   open ( 1, file = 'structConstr001alk.1', form='unformatted', access='sequential' )

   write ( 1, iostat = stat, iomsg = msg )        base(3)() ! tcx: (3)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 1_4

   write ( 1, iostat = stat, iomsg = msg )        child(3,4)() ! tcx: (3,4)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 2_4

   write ( 1, iostat = stat, iomsg = msg )        gen3(3,4)() ! tcx: (3,4)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteg' ) ) error stop 3_4

   write ( 1, iostat = stat, iomsg = msg )        base(3)('abc') ! tcx: (3)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 4_4

   write ( 1, iostat = stat, iomsg = msg )        child(3,4)('def',1001) ! tcx: (3,4)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 5_4

   write ( 1, iostat = stat, iomsg = msg )        gen3(3,4)('ghi',10002,'jkl') ! tcx: (3,4)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteg' ) ) error stop 6_4

   rewind 1

   read ( 1, iostat= stat, iomsg = msg ) b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 7_4
   read ( 1, iostat= stat, iomsg = msg ) c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 8_4
   read ( 1, iostat= stat, iomsg = msg ) g1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 9_4

   if ( ( b1%c /= 'xxx' ) .or. &
        ( c1%c /= 'xxx' ) .or. ( c1%i /= -999 ) .or. &
        ( g1%c /= 'xxx' ) .or. ( g1%i /= -999 ) .or. ( g1%s /= 'xxx' ) ) error stop 10_4

end program

subroutine read (dtv, unit, iostat, iomsg)
   use m, only: base, child, gen3
   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type ( dtv )
      type is ( base(*) ) ! tcx: (*)
         read (unit, iostat=iostat, iomsg=iomsg) dtv%c
      type is ( child(*,4) ) ! tcx: (*,4)
         read (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
      type is ( gen3(*,4) ) ! tcx: (*,4)
         read (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i, dtv%s
   end select

   iomsg = 'dtioread'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase1) to invoke with (3) / declare with (*) - 7 changes
! type: child - added parameters (kchild1) to invoke with (3,4) / declare with (*,4) - 5 changes
! type: gen3 - added parameters () to invoke with (3,4) / declare with (*,4) - 5 changes
