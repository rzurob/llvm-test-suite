!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Function Return
!*                                    -  function result is a (non-)polymorphic array entity with unformatted i/o
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
         procedure, pass :: write => writeb
         generic :: write(unformatted) => write
   end type

   type, extends(base) :: child
      integer(4) :: i = -999
      contains
         procedure, pass :: write => writec
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

      class(base) function returnMe(dtv)
         class(base), intent(in) :: dtv(:)
         allocatable :: returnMe(:)

         allocate ( returnMe(size(dtv,1)), source = dtv )

      end function

end module

program funcRetrn002a
   use m

   integer :: stat
   character(200) :: msg
   character(9) :: cc1
   character(12) :: cc2
   character(3) :: cc3, cc4, cc5
   integer :: i1, i2, i3

   type(base), pointer     :: b1(:)
   class(base), allocatable :: b2(:)
   class(child), allocatable, target :: c1(:)

   interface
      type(base) function returnMeExt(dtv)
         import base
         type(base), intent(in) :: dtv(3)
         dimension :: returnMeExt(3)
      end function
   end interface

   allocate ( b1(3), source = (/ base('abc'), base('def'), base('ghi') /) )
   allocate ( b2(4), source = (/ base('ABC'), base('DEF'), base('GHI'), base('JKL') /) )
   allocate ( c1(3), source = (/ child ( 'abc', 10001 ), child ( 'def', 10002 ), child ( 'ghi', 10003 ) /) )

   open ( 1, file = 'funcRetrn002a.1', form='unformatted', access='sequential' )

   write ( 1, iostat = stat, iomsg = msg )       returnMeExt(b1)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )        error stop 1_4

   write ( 1, iostat = stat, iomsg = msg )       returnMe(b2)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )        error stop 2_4

   write ( 1, iostat = stat, iomsg = msg )       returnMe(c1)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )        error stop 3_4

   rewind 1

   read ( 1, iostat = stat, iomsg = msg )        cc1
   read ( 1, iostat = stat, iomsg = msg )        cc2
   read ( 1, iostat = stat, iomsg = msg )        cc3, i1, cc4, i2, cc5, i3

   if ( ( cc1 /= 'abcdefghi' ) .or. ( cc2 /= 'ABCDEFGHIJKL' ) .or. &
        ( cc3 /= 'abc' ) .or. ( i1 /= 10001 ) .or. &
        ( cc4 /= 'def' ) .or. ( i2 /= 10002 ) .or. &
        ( cc5 /= 'ghi' ) .or. ( i3 /= 10003 ) ) error stop 4_4

   close ( 1, status = 'delete' )

end program

type(base) function returnMeExt(dtv)
  use m, only: base
  type(base), intent(in) :: dtv(3)
  dimension :: returnMeExt(3)
  returnMeExt = dtv
end function
