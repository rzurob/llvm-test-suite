!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Section 4.5.4 Type Bound Procedures (generic-binding)
!*                                 - Specific Binding
!*                                    - deferred specific type bound procedure
!*                                         - deferred binding in parent type, and child
!*                                           type has specific type bound in external procedure
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

   type, abstract :: base
      character(3) :: c = 'xxx'
      contains
         procedure(wbinf), deferred, pass :: write
         procedure(rbinf), deferred, pass :: read
         generic :: write(unformatted) => write
         generic :: read(unformatted)  => read
   end type

   type, extends(base) :: child
      integer(4) :: i = -999
      contains
         procedure, pass :: write
         procedure, pass :: read
   end type

   interface
      subroutine wbinf (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   interface
      subroutine rbinf (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   interface
      subroutine write (dtv, unit, iostat, iomsg)
         import child
         class(child), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   interface
      subroutine read (dtv, unit, iostat, iomsg)
         import child
         class(child), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

end module

program specific010
   use m

   integer(4) :: stat
   character(200) :: msg

   class(base), allocatable  :: b1
   class(child), pointer     :: c1
   type(child)               :: c2 = child ( 'ghi', 1003 )

   allocate ( b1, source = child ( 'abc', 1001 ) )
   allocate ( c1, source = child ( 'def', 1002 ) )

   open ( 1, file = 'specific010.1', form='unformatted', access='sequential' )

   write ( 1, iostat = stat, iomsg = msg ) b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 1_4

   write ( 1, iostat = stat, iomsg = msg ) c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 2_4

   write ( 1, iostat = stat, iomsg = msg ) c2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 3_4

   rewind 1

   read ( 1, iostat = stat, iomsg = msg ) c2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) ) error stop 4_4

   read ( 1, iostat = stat, iomsg = msg ) b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) ) error stop 5_4

   read ( 1, iostat = stat, iomsg = msg ) c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) ) error stop 6_4

   select type ( b1 )
      type is ( child )
         if ( ( c2%c /= 'abc' ) .or. ( c2%i /= 1001 ) .or. ( b1%c /= 'def' ) .or. ( b1%i /= 1002 ) .or. &
              ( c1%c /= 'ghi' ) .or. ( c1%i /= 1003 )  ) error stop 7_4
   end select

   close (1, status = 'delete' )

end program

subroutine write (dtv, unit, iostat, iomsg)
   use m, only: child
   class(child), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
   iomsg = 'dtiowritec'

end subroutine

subroutine read (dtv, unit, iostat, iomsg)
   use m, only: child
   class(child), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
   iomsg = 'dtioreadc'

end subroutine

