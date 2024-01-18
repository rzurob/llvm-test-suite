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
!*                                  Cross Feature: Function Return
!*                                    -  function result is a unlimited polymorphic scalar/array entity with unformatted i/o
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

      class(*) function returnMeScalar(dtv)
         class(*), intent(in) :: dtv
         allocatable :: returnMeScalar

         allocate ( returnMeScalar, source = dtv )

      end function

      class(*) function returnMe(dtv)
         class(*), intent(in) :: dtv(:)
         allocatable :: returnMe(:)

         allocate ( returnMe(size(dtv,1)), source = dtv )

      end function

end module

program funcRetrn003a
   use m

   integer :: stat
   character(200) :: msg

   type(base), pointer     :: b1(:)
   class(base), allocatable :: b2(:)
   class(child), allocatable, target :: c1(:)
   character(9) :: cc1
   character(12) :: cc2
   character(3) :: cc3, cc4, cc5, cc6, cc7
   integer :: i1, i2, i3, i4

   interface
      class(*) function returnMeExt(dtv)
         class(*), intent(in) :: dtv(3)
         allocatable :: returnMeExt(:)
      end function
   end interface

   allocate ( b1(3), source = (/ base('abc'), base('def'), base('ghi') /) )
   allocate ( b2(4), source = (/ base('ABC'), base('DEF'), base('GHI'), base('JKL') /) )
   allocate ( c1(3), source = (/ child ( 'abc', 10001 ), child ( 'def', 10002 ), child ( 'ghi', 10003 ) /) )

   open ( 1, file = 'funcRetrn003a.1', form='unformatted', access='sequential' )

   select type ( g => returnMeExt(b1) )
      class is ( base )
         write ( 1, iostat = stat, iomsg = msg )       g
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )        error stop 1_4
   end select

   select type ( h => returnMe(b2) )
      type is ( base )
         write ( 1, iostat = stat, iomsg = msg )       h
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )        error stop 2_4
   end select

   select type ( i => returnMe(c1) )
      class is ( base )
         write ( 1, iostat = stat, iomsg = msg )  i
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )        error stop 3_4
   end select

   select type ( g => returnMeScalar(b1(1)) )
      class is ( base )
         write ( 1, iostat = stat, iomsg = msg )       g
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )        error stop 4_4
   end select

   select type ( g => returnMeScalar(c1(3)) )
      class is ( base )
         write ( 1, iostat = stat, iomsg = msg )       g
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )        error stop 5_4
   end select

   rewind 1

   read ( 1, iostat = stat, iomsg = msg )        cc1
   read ( 1, iostat = stat, iomsg = msg )        cc2
   read ( 1, iostat = stat, iomsg = msg )        cc3, i1, cc4, i2, cc5, i3
   read ( 1, iostat = stat, iomsg = msg )        cc6
   read ( 1, iostat = stat, iomsg = msg )        cc7, i4

   if ( ( cc1 /= 'abcdefghi' ) .or. ( cc2 /= 'ABCDEFGHIJKL' ) .or. &
        ( cc3 /= 'abc' ) .or. ( i1 /= 10001 ) .or. &
        ( cc4 /= 'def' ) .or. ( i2 /= 10002 ) .or. &
        ( cc5 /= 'ghi' ) .or. ( i3 /= 10003 ) .or. &
        ( cc6 /= 'abc' ) .or. ( cc7 /= 'ghi' ) .or. ( i4 /= 10003 ) ) error stop 6_4

end program

class(*) function returnMeExt(dtv)
  use m, only: base
  class(*), intent(in) :: dtv(3)
  allocatable :: returnMeExt(:)
  allocate ( returnMeExt(size(dtv,1)), source = dtv )
end function
