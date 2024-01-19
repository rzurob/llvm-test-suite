!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Section 9.5.3.7.3 Resolving derived-type input/output procedure references (generic binding)
!*                                    - Make both generic type bound and interface available
!*                                        - for formatted I/O with single non-extended type
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
         procedure, pass :: write => writebase
         procedure, pass :: read => readbase
         generic :: write(formatted) => write
         generic :: read(formatted)  => read
   end type

   contains

      subroutine writebase (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3)", iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtiowrite'

      end subroutine

      subroutine readbase (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         if ( iotype == "LISTDIRECTED" ) then
            read (unit, "(1X,A3)" , iostat=iostat, iomsg=iomsg) dtv%c
         else
            read (unit, "(A3)" , iostat=iostat, iomsg=iomsg) dtv%c
         end if

         iomsg = 'dtioread'

      end subroutine

end module

program resolve001
   use m

   interface write(formatted)
      subroutine writebaseext (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   interface read(formatted)
      subroutine readbaseext (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   class(base), allocatable :: b1
   type(base) , pointer     :: b2

   namelist /n1/ b1, b2

   integer :: stat
   character(200) :: msg

   allocate ( b1, source = base ( 'abc' ) )
   allocate ( b2, source = base ( 'def' ) )

   open ( 1, file = 'resolve001.1', form='formatted', access='sequential' )

   write ( 1, n1, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )     error stop 1_4

   write ( 1, * , iostat = stat, iomsg = msg ) b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )     error stop 2_4

   write ( 1, "(1X,DT)", iostat = stat, iomsg = msg ) b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )     error stop 3_4

   rewind 1

   b1%c = 'xxx'
   b2%c = 'xxx'

   read ( 1, n1, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )     error stop 4_4
   if ( ( b1%c /= 'abc' ) .or. ( b2%c /= 'def' ) )     error stop 5_4

   b2%c = 'xxx'

   read ( 1, * , iostat = stat, iomsg = msg ) b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )     error stop 6_4
   if ( ( b2%c /= 'abc' ) )     error stop 7_4

   b2%c = 'xxx'

   read ( 1, "(T2,DT)", iostat = stat, iomsg = msg ) b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )     error stop 8_4
   if ( ( b2%c /= 'abc' ) )     error stop 9_4

   close ( 1, status ='delete')

end program

subroutine writebaseext (dtv, unit, iotype, v_list, iostat, iomsg)
   use m
   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)  :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   error stop 10_4
   iomsg = 'ERROR'

end subroutine

subroutine readbaseext (dtv, unit, iotype, v_list, iostat, iomsg)
   use m
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)  :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   error stop 11_4
   iomsg = 'ERROR'

end subroutine
