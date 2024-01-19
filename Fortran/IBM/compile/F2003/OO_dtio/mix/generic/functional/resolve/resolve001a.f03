!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Section 9.5.3.7.3 Resolving derived-type input/output procedure references (generic binding)
!*                                    - Make both generic type bound and interface available
!*                                        - for unformatted I/O with single non-extended type
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
         generic :: write(unformatted) => write
         generic :: read(unformatted)  => read
   end type

   contains

      subroutine writebase (dtv, unit, iostat, iomsg)
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtiowrite'

      end subroutine

      subroutine readbase (dtv, unit, iostat, iomsg)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtioread'

      end subroutine

end module

program resolve001a
   use m

   interface write(unformatted)
      subroutine writebaseext (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   interface read(unformatted)
      subroutine readbaseext (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
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

   open ( 1, file = 'resolve001a.1', form='unformatted', access='sequential' )

   write ( 1, iostat = stat, iomsg = msg ) b1, b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )     error stop 1_4

   rewind 1

   read ( 1, iostat = stat, iomsg = msg ) b2, b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )      error stop 2_4
   if ( ( b1%c /= 'def' ) .or. ( b2%c /= 'abc' ) )      error stop 3_4

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

   error stop 4_4
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

   error stop 5_4
   iomsg = 'ERROR'

end subroutine
