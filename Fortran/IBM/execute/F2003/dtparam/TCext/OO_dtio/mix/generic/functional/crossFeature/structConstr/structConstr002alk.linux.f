!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : structConstr002a.linuxlk
!*
!*  PROGRAMMER                 : David Forster (derived from structConstr002a.linux by Robert Ma)
!*  DATE                       : 2007-08-08 (original: 04/26/2005)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Structure Constructor
!*                                    -  Structure constructor with allocatable components and io-implied-do unformatted i/o
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
      character(lbase1), allocatable :: c
      contains
         procedure, pass :: write => writeb
         generic :: write(unformatted) => write
   end type

   type, extends(base) :: child (kchild1) ! kchild1=4
      integer, kind :: kchild1
      integer(kchild1), allocatable :: i
      contains
         procedure, pass :: write => writec
   end type

   type, extends(child) :: gen3
      character(lbase1), allocatable :: s
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

         write (unit,  iostat=iostat, iomsg=iomsg) dtv%c, dtv%i, dtv%s

         iomsg = 'dtiowriteg'

      end subroutine

end module

program structConstr002a
   use m

   integer :: stat
   character(200) :: msg

   type(base(3))  :: b1 ! tcx: (3)
   type(child(3,4)) :: c1 ! tcx: (3,4)
   type(gen3(3,4))  :: g1 ! tcx: (3,4)

   interface read(unformatted)
      subroutine myread (dtv, unit, iostat, iomsg)
         import base
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface read(unformatted)

   open ( 1, file = 'structConstr002a.1', form='unformatted', access='sequential' )

   write ( 1, iostat = stat, iomsg = msg )        base(3)('abc') ! tcx: (3)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 1_4

   write ( 1, iostat = stat, iomsg = msg )      child(3,4)('def',1001) ! tcx: (3,4)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 2_4

   write ( 1,  iostat = stat, iomsg = msg )    gen3(3,4)('ghi',10002,'jkl') ! tcx: (3,4)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteg' ) ) error stop 3_4

   write ( 1, iostat = stat, iomsg = msg )  ( base(3)('ABC'), base(3)('DEF'), i=0,1 ) ! tcx: (3) ! tcx: (3)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 4_4

   rewind 1

   allocate ( b1%c, c1%c, c1%i, g1%c, g1%i, g1%s )

   read ( 1, iostat= stat, iomsg = msg ) b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 5_4
   read ( 1, iostat= stat, iomsg = msg ) c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 6_4
   read ( 1, iostat= stat, iomsg = msg ) g1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 7_4

   if ( ( b1%c /= 'abc' ) .or. &
        ( c1%c /= 'def' ) .or. ( c1%i /= 1001 ) .or. &
        ( g1%c /= 'ghi' ) .or. ( g1%i /= 10002 ) .or. ( g1%s /= 'jkl' ) ) error stop 8_4

   read ( 1, iostat = stat, iomsg = msg )         ( b1, b1, i=0,1 )  !<- read 4 times b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 9_4
   if ( b1%c /= 'DEF' )  error stop 10_4
   

end program

subroutine myread (dtv, unit, iostat, iomsg)
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
! type: base - added parameters (lbase1) to invoke with (3) / declare with (*) - 8 changes
! type: child - added parameters (kchild1) to invoke with (3,4) / declare with (*,4) - 4 changes
! type: gen3 - added parameters () to invoke with (3,4) / declare with (*,4) - 4 changes
