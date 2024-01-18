!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Alberto Alvarez-Mesquida
!*  DATE                       : 02/20/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Section 4.5.4 Type Bound Procedures (generic
!*                               -binding)
!*                               - Specific Binding
!*                               - deferred specific type bound procedure
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
         procedure(winf), deferred, pass :: write
         procedure(rinf), deferred, pass :: read
         generic :: write(formatted) => write
         generic :: read(formatted)  => read
   end type

   type, extends(base) :: child
      integer(4) :: i = -999
      contains
         procedure, pass :: write => writechild
         procedure, pass :: read  => readchild
   end type

   abstract interface
      subroutine winf (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   abstract interface
      subroutine rinf (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   contains

      subroutine writechild (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
         iomsg = 'dtiowrite'

      end subroutine

      subroutine readchild (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(A3,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
         iomsg = 'dtioread'

      end subroutine

end module

program abstracti006
   use m

   integer(4) :: stat
   character(200) :: msg


   class(base), allocatable :: b1
   class(base), pointer     :: b2

   type(child), target      :: c1
   class(child), pointer    :: c2

   namelist /n1/ b1, c1
   namelist /n2/ b2, c2

   allocate ( b1, source = child ( 'abc', 1001 ) )
   allocate ( b2, source = child ( 'def', 1002 ) )
   c1 = child ( 'ghi', 1003 )
   allocate ( c2, source = child ( 'jkl', 1004 ) )

   open ( 1, file = 'abstracti006.1', form='formatted', access='sequential' )

   write ( 1, n1, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write ( 1, n2, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   rewind 1

   deallocate ( b1, b2, c2 )
   allocate ( child :: b1, b2, c2 )
   c1 = child()

   read ( 1, n1, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4

   read ( 1, n2, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 4_4

   select type ( b1 )
      type is ( child )
         if ( ( b1%c /= 'abc' ) .or. (b1%i /= 1001 ) ) error stop 5_4
   end select

   select type ( b2 )
      type is ( child )
         if ( ( b2%c /= 'def' ) .or. (b2%i /= 1002 ) ) error stop 6_4
   end select

   if ( ( c1%c /= 'ghi' ) .or. ( c1%i /= 1003 ) ) error stop 7_4
   if ( ( c2%c /= 'jkl' ) .or. ( c2%i /= 1004 ) ) error stop 8_4

   deallocate ( b2, c2 )
   allocate ( c2 )
   b2 => c2

   rewind 1

   read ( 1, n2, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )     error stop 9_4

   select type ( b2 )
      type is ( child )
         if ( ( b2%c /= 'jkl' ) .or. (b2%i /= 1004 ) ) error stop 10_4
   end select

   if ( ( c2%c /= 'jkl' ) .or. ( c2%i /= 1004 ) )      error stop 11_4

end program abstracti006
