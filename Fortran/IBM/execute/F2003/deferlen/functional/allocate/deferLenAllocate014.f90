!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 05/01/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DRIVER STANZA              : xlf2003
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
      character(:), allocatable :: c1
      contains
         procedure :: un_write
         procedure :: un_read
         generic :: write(unformatted) => un_write
         generic :: read(unformatted) => un_read
   end type

   type, extends(base) :: child
      integer(2) :: i
      character(:), allocatable :: c2
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

program deferLenAllocate014
   use m

   integer :: stat
   character(150) :: msg = ''

   type(base) :: b1
   class(base) :: b2
   allocatable :: b2


   open( 1, file ='deferLenAllocate014.1', access='sequential', form='unformatted' )
   write ( 1, iostat = stat, iomsg = msg ) base('abcdefghi'), child('IBM', 2003, 'FORTRAN')
   if ( ( stat /= 0 ) .or. ( msg /= '' ) ) error stop 1_4

   rewind 1

   allocate ( b1%c1, source = '         ' )
   allocate ( b2, source = child ( '   ', -999, '       ' ) )

   read ( 1, iostat = stat, iomsg = msg ) b1, b2

   select type ( b2 )
      type is ( child )
         if ( ( b1%c1 /= 'abcdefghi' ) .or. ( b2%c1 /= 'IBM' ) .or. ( b2%i /= 2003 ) .or. ( b2%c2 /= 'FORTRAN' ) ) error stop 2_4
   end select

   if ( ( stat /= 0 ) .or. ( msg /= '' ) ) error stop 3_4
   
   close ( 1, status = 'delete' )

end program
