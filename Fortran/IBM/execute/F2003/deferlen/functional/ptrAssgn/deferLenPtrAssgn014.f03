!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : scalar character with deferred length
!*                               with unformatted DT I/O (namelist)
!*
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
      character(:), pointer :: c1
      contains
         procedure :: un_write
         procedure :: un_read
         generic :: write(unformatted) => un_write
         generic :: read(unformatted) => un_read
   end type

   type, extends(base) :: child
      integer(2) :: i
      character(:), pointer :: c2
   end type

   contains

      subroutine un_write ( dtv, unit, iostat, iomsg )
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write ( unit, iostat = iostat, iomsg = iomsg ) dtv%c1

         select type ( dtv )
            type is ( child )
               write ( unit, iostat = iostat, iomsg = iomsg ) dtv%i, dtv%c2
         end select

      end subroutine

      subroutine un_read ( dtv, unit, iostat, iomsg )
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read ( unit, iostat = iostat, iomsg = iomsg ) dtv%c1

         select type ( dtv )
            type is ( child )
               read ( unit, iostat = iostat, iomsg = iomsg ) dtv%i, dtv%c2
         end select

      end subroutine

end module

program deferLenPtrAssgn014
   use m

   integer :: stat
   character(150) :: msg = ''

   type(base) :: b1
   class(base) :: b2
   allocatable :: b2

   character(9), target :: c1, c2, c3

   open( 1, file ='deferLenPtrAssgn014.1', access='sequential', form='unformatted' )

   c1 = "abcdefghi"
   c2 = "ABCDEFGHI"
   c3 = "JKLMNOPQR"

   allocate ( b2 )

   b1%c1 => c1
   b2%c1 => c2

   select type ( b2 )
      type is ( child )
         b2%c2 => c3
         b2%i = 2003
   end select

   write ( 1, iostat = stat, iomsg = msg ) b1, b2
   if ( ( stat /= 0 ) .or. ( msg /= '' ) ) error stop 1_4

   rewind 1

   allocate ( character(9) :: b1%c1 )

   select type ( b2 )
      type is ( child )
         allocate ( character(9) :: b2%c1 )
         b2%c2 => b2%c1
   end select

   read ( 1, iostat = stat, iomsg = msg ) b1, b2

   select type ( b2 )
      type is ( child )
         if ( ( b1%c1 /= 'abcdefghi' ) .or. ( b2%c1 /= 'JKLMNOPQR' ) .or. ( b2%i /= 2003 ) .or. ( b2%c2 /= 'JKLMNOPQR' ) ) error stop 2_4
   end select

   if ( ( stat /= 0 ) .or. ( msg /= '' ) ) error stop 3_4

   close ( 1, status = 'delete' )

end program
