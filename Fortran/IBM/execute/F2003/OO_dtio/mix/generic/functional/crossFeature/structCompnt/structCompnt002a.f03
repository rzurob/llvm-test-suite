!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Structure Component
!*                                    -  non-polymorphic structure component with unformatted i/o
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
         procedure, pass :: read => readb
         generic :: write(unformatted) => write
         generic :: read(unformatted) => read
   end type

   type container
      type(base) :: b1
      type(base), allocatable :: b2
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

      subroutine readb (dtv, unit, iostat, iomsg)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtioreadb'

      end subroutine

end module

program structCompnt002
   use m

   integer :: stat
   character(200) :: msg

   type(container) :: cc1
   class(container), allocatable :: cc2

   open ( 1, file = 'structCompnt002.1', form='unformatted', access='sequential' )

   cc1 = container( base('abc'), base('def') )
   allocate ( cc2, source = container( base('ABC'), base('DEF') ) )

   write ( 1, iostat = stat, iomsg = msg )                   cc1%b1, cc1%b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )         error stop 1_4

   write ( 1, iostat = stat, iomsg = msg )                   cc2%b1, cc2%b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )         error stop 2_4

   rewind 1

   read ( 1, iostat = stat, iomsg = msg )                    cc2%b1, cc2%b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )          error stop 3_4

   if ( ( cc2%b1%c /= 'abc' ) .or. ( cc2%b2%c /= 'def' ) )   error stop 4_4

   read ( 1, iostat = stat, iomsg = msg )                    cc1%b1, cc1%b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )          error stop 5_4

   if ( ( cc1%b1%c /= 'ABC' ) .or. ( cc1%b2%c /= 'DEF' ) )   error stop 6_4

   close ( 1, status ='delete')

end program