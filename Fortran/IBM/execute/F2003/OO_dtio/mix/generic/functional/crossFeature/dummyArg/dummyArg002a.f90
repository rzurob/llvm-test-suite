!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 04/26/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Dummy Argument Association
!*                                    - Dummy Argument being polymorphic scalar entity with unformatted i/o
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
         generic :: write(unformatted) => write
         generic :: read(unformatted)  => read
   end type

   type, extends(base) :: child
      integer(4) :: i = -999
   end type

   interface
      subroutine mywriteext ( dtv )
         import base
         class(base), pointer, intent(in) :: dtv
      end subroutine
   end interface

   interface
      subroutine myreadext ( dtv )
         import base
         class(base), allocatable, intent(inout) :: dtv
      end subroutine
   end interface

   contains

      subroutine write (dtv, unit, iostat, iomsg)
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         select type ( dtv )
            type is ( base )
               write (unit, iostat=iostat, iomsg=iomsg) dtv%c
               iomsg = 'dtiowriteb'
            type is ( child )
               write (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
               iomsg = 'dtiowritec'
         end select

      end subroutine

      subroutine read (dtv, unit, iostat, iomsg)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         select type ( dtv )
            type is ( base )
               read (unit, iostat=iostat, iomsg=iomsg) dtv%c
               iomsg = 'dtioreadb'
            type is ( child )
               read (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
               iomsg = 'dtioreadc'
         end select

      end subroutine

      subroutine mywrite ( dtv )
         class(base), intent(in) :: dtv
         integer :: stat
         character(200) :: msg

         write ( 1, iostat = stat, iomsg = msg ) dtv
         select type ( dtv )
            type is ( base )
               if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 1_4
            type is ( child )
               if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 2_4
         end select

      end subroutine

      subroutine myread ( dtv )
         class(base), intent(inout) :: dtv
         integer :: stat
         character(200) :: msg

         read ( 1, iostat = stat, iomsg = msg) dtv
         select type ( dtv )
            type is ( base )
               if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) ) error stop 3_4
            type is ( child )
               if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) ) error stop 4_4
         end select

      end subroutine

end module

program dummyArg002a
   use m

   class(base), allocatable :: b1
   class(base), pointer     :: b2

   open ( 1, file='dummyArg002a.1', form='unformatted', access='sequential' )

   allocate ( b1, source = base('abc') )
   allocate ( b2, source = base('def') )

   call mywrite ( b1 )
   call mywrite ( b2 )
   call mywriteext ( b2 )

   deallocate ( b1, b2 )
   allocate ( b1, source = child ( 'ABC', 101 ) )
   allocate ( b2, source = child ( 'DEF', 102 ) )

   call mywrite ( b1 )
   call mywrite ( b2 )
   call mywriteext ( b2 )

   rewind 1

   deallocate ( b1, b2 )
   allocate ( b1, b2 )

   call myread  ( b1 )
   call myread  ( b2 )

   if ( ( b1%c /= 'abc' ) .or. ( b2%c /= 'def' )  ) error stop 5_4

   call myreadext ( b1 )

   if ( b1%c /= 'def' ) error stop 6_4

   deallocate ( b1, b2 )
   allocate ( child :: b1, b2 )

   call myread  ( b1 )
   call myread  ( b2 )

   select type ( b1 )
      type is ( child )
         select type ( b2 )
            type is ( child )
               if ( ( b1%c /= 'ABC' ) .or. ( b1%i /= 101 ) .or. ( b2%c /= 'DEF' ) .or. ( b2%i /= 102 )  ) error stop 7_4
         end select
   end select

   call myreadext ( b1 )

   select type ( b1 )
      type is ( child )
         if ( ( b1%c /= 'DEF' ) .or. ( b1%i /= 102 )  ) error stop 8_4
   end select

   close ( 1, status ='delete')

end program

subroutine mywriteext ( dtv )
   use m, only: base, child
   class(base), pointer, intent(in) :: dtv
   integer :: stat
   character(200) :: msg

   write ( 1, iostat = stat, iomsg = msg ) dtv
   
   select type ( dtv )
      type is ( base )
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 9_4
      type is ( child )
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 10_4
   end select

end subroutine

subroutine myreadext ( dtv )
   use m, only: base, child
   class(base), allocatable, intent(inout) :: dtv
   integer :: stat
   character(200) :: msg

   read ( 1, iostat = stat, iomsg = msg ) dtv
   
   select type ( dtv )
      type is ( base )
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) ) error stop 11_4
      type is ( child )
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) ) error stop 12_4
   end select

end subroutine
