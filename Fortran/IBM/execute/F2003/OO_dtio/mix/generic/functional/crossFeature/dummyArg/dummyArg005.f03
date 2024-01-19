!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Dummy Argument Association
!*                                    - Dummy Argument being polymorphic array entity with formatted i/o
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
      character(3) :: c = 'xxx'
      contains
         procedure, pass :: write
         procedure, pass :: read
         generic :: write(formatted) => write
         generic :: read(formatted)  => read
   end type

   type, extends(base) :: child
      integer(4) :: i = -999
      contains
         procedure, pass :: write => writec
         procedure, pass :: read => readc
   end type

   interface
      subroutine mywriteext ( dtv )
         import base
         class(base), pointer, intent(in) :: dtv(:)
      end subroutine
   end interface

   interface
      subroutine myreadext ( dtv )
         import base
         class(base), allocatable, intent(inout) :: dtv(:)
      end subroutine
   end interface

   contains

      subroutine write (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3)", iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtiowriteb'

      end subroutine

      subroutine read (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(A3)", iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtioreadb'

      end subroutine


      subroutine writec (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
         iomsg = 'dtiowritec'

      end subroutine

      subroutine readc (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(A3,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
         iomsg = 'dtioreadc'

      end subroutine

      subroutine mywrite ( dtv )
         class(base), intent(in) :: dtv(3)
         integer :: stat
         character(200) :: msg

         write ( 1, *, iostat = stat, iomsg = msg ) dtv
         select type ( dtv )
            type is ( base )
               if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 1_4
            type is ( child )
               if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 2_4
         end select

      end subroutine

      subroutine myread ( dtv )
         class(base), intent(inout) :: dtv(:)
         integer :: stat
         character(200) :: msg

         read ( 1, "(3(1x,DT))", iostat = stat, iomsg = msg) dtv
         select type ( dtv )
            type is ( base )
               if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) ) error stop 3_4
            type is ( child )
               if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) ) error stop 4_4
         end select

      end subroutine

end module

program dummyArg005
   use m

   class(base), allocatable :: b1(:)
   class(base), pointer     :: b2(:)

   open ( 1, file='dummyArg005.1', form='formatted', access='sequential' )

   allocate ( b1(3), source = (/ base('abc'), base('def'), base('ghi') /) )
   allocate ( b2(2:4), source = (/ base('ABC'), base('DEF'), base('GHI') /) )

   call mywrite ( b1 )
   call mywrite ( b2(2:4) )
   call mywriteext ( b2 )

   deallocate ( b1, b2 )
   allocate ( b1(2:4), source = (/ child ( 'ABC', 101 ), child ( 'DEF', 102 ), child ( 'GHI', 103 ) /) )
   allocate ( b2(3:5), source = (/ child ( 'abc', 201 ), child ( 'def', 202 ), child ( 'ghi', 203 ) /) )

   call mywrite ( b1(2:4) )
   call mywrite ( b2 )
   call mywriteext ( b2 )

   rewind 1

   deallocate ( b1, b2 )
   allocate ( b1(3), b2(3) )

   call myread  ( b1 )
   call myread  ( b2 )

   if ( ( b1(1)%c /= 'abc' ) .or. ( b1(2)%c /= 'def' ) .or. ( b1(3)%c /= 'ghi' ) .or. &
        ( b2(1)%c /= 'ABC' ) .or. ( b2(2)%c /= 'DEF' ) .or. ( b2(3)%c /= 'GHI' )  ) error stop 5_4

   call myreadext ( b1 )

   if ( ( b1(1)%c /= 'ABC' ) .or. ( b1(2)%c /= 'DEF' ) .or. ( b1(3)%c /= 'GHI' )  ) error stop 6_4

   deallocate ( b1, b2 )
   allocate ( child :: b1(3), b2(3) )

   call myread  ( b1 )
   call myread  ( b2 )

   select type ( b1 )
      type is ( child )
         select type ( b2 )
            type is ( child )
               if ( ( b1(1)%c /= 'ABC' ) .or. ( b1(1)%i /= 101 ) .or. &
                    ( b1(2)%c /= 'DEF' ) .or. ( b1(2)%i /= 102 ) .or. &
                    ( b1(3)%c /= 'GHI' ) .or. ( b1(3)%i /= 103 ) .or. &
                    ( b2(1)%c /= 'abc' ) .or. ( b2(1)%i /= 201 ) .or. &
                    ( b2(2)%c /= 'def' ) .or. ( b2(2)%i /= 202 ) .or. &
                    ( b2(3)%c /= 'ghi' ) .or. ( b2(3)%i /= 203 ))      error stop 7_4
         end select
   end select

   call myreadext ( b1 )

   select type ( b1 )
      type is ( child )
         if ( ( b1(1)%c /= 'abc' ) .or. ( b1(1)%i /= 201 ) .or. &
              ( b1(2)%c /= 'def' ) .or. ( b1(2)%i /= 202 ) .or. &
              ( b1(3)%c /= 'ghi' ) .or. ( b1(3)%i /= 203 )  ) error stop 8_4
   end select

   close ( 1, status ='delete')

end program

subroutine mywriteext ( dtv )
   use m, only: base, child
   class(base), pointer, intent(in) :: dtv(:)
   integer :: stat
   character(200) :: msg

   write ( 1, *, iostat = stat, iomsg = msg ) dtv

   select type ( dtv )
      type is ( base )
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 9_4
      type is ( child )
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 10_4
   end select

end subroutine

subroutine myreadext ( dtv )
   use m, only: base, child
   class(base), allocatable, intent(inout) :: dtv(:)
   integer :: stat
   character(200) :: msg

   read ( 1, "(3(1X,DT))" , iostat = stat, iomsg = msg ) dtv

   select type ( dtv )
      type is ( base )
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) ) error stop 11_4
      type is ( child )
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) ) error stop 12_4
   end select

end subroutine

