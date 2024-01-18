!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Function Return
!*                                    -  function result is a (non-)polymorphic scalar entity with unformatted i/o
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
         procedure, pass :: returnMe
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
         class(base), intent(in) :: dtv
         allocatable :: returnMe

         allocate ( returnMe, source = dtv )

      end function

end module

program funcRetrn001a
   use m

   integer :: stat
   character(200) :: msg
   character(3) :: cc1, cc2, cc3
   integer :: i1

   type(base), pointer     :: b1
   class(base), allocatable :: b2
   class(child), allocatable, target :: c1

   procedure ( type(base) ) :: returnMeExt

   allocate ( b1, source = base ( 'abc' ) )
   allocate ( b2, source = base ( 'def' ) )
   allocate ( c1, source = child ( 'ghi', 10001 ) )

   open ( 1, file = 'funcRetrn001a.1', form='unformatted', access='sequential' )

   write ( 1, iostat = stat, iomsg = msg )    returnMeExt(b1)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 1_4

   write ( 1, iostat = stat, iomsg = msg )    returnMe(b2)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 2_4

   write ( 1, iostat = stat, iomsg = msg )  c1%returnMe()
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 3_4

   rewind 1

   read ( 1, iostat = stat, iomsg = msg )    cc1
   read ( 1, iostat = stat, iomsg = msg )    cc2
   read ( 1, iostat = stat, iomsg = msg )    cc3, i1
   if ( ( cc1 /= 'abc' ) .or. ( cc2 /= 'def' ) .or. ( cc3 /= 'ghi' ) .or. ( i1 /= 10001 ) )  error stop 4_4


end program

type(base) function returnMeExt(dtv)
  use m, only: base
  type(base), intent(in) :: dtv
  returnMeExt = dtv
end function
