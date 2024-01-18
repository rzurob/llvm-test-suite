!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
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

   type base
      character(3), allocatable :: c
      contains
         procedure, pass :: write => writeb
         generic :: write(unformatted) => write
   end type

   type, extends(base) :: child
      integer(4), allocatable :: i
      contains
         procedure, pass :: write => writec
   end type

   type, extends(child) :: gen3
      character(3), allocatable :: s
      contains
         procedure, pass :: write => writeg
   end type

   contains

      subroutine writeb (dtv, unit, iostat, iomsg)
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtiowriteb'

      end subroutine

      subroutine writec (dtv, unit, iostat, iomsg)
         class(child), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i

         iomsg = 'dtiowritec'

      end subroutine

      subroutine writeg (dtv, unit, iostat, iomsg)
         class(gen3), intent(in) :: dtv
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

   type(base)  :: b1
   type(child) :: c1
   type(gen3)  :: g1

   interface read(unformatted)
      subroutine read (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface read(unformatted)

   open ( 1, file = 'structConstr002a.1', form='unformatted', access='sequential' )

   write ( 1, iostat = stat, iomsg = msg )        base('abc')
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 1_4

   write ( 1, iostat = stat, iomsg = msg )      child('def',1001)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 2_4

   write ( 1,  iostat = stat, iomsg = msg )    gen3('ghi',10002,'jkl')
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteg' ) ) error stop 3_4

   write ( 1, iostat = stat, iomsg = msg )  ( base('ABC'), base('DEF'), i=0,1 )
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

subroutine read (dtv, unit, iostat, iomsg)
   use m, only: base, child, gen3
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type ( dtv )
      type is ( base )
         read (unit, iostat=iostat, iomsg=iomsg) dtv%c
      type is ( child )
         read (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
      type is ( gen3 )
         read (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i, dtv%s
   end select

   iomsg = 'dtioread'

end subroutine

