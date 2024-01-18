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
!*  DESCRIPTION                : Section 4.5.4 Type Bound Procedures (generic-binding)
!*                                 - Specific Binding
!*                                    - deferred specific type bound procedure (with Array)
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

   interface
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

   interface
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

program specific002a
   use m

   integer(4) :: stat
   character(200) :: msg


   class(base), allocatable :: b1(:)
   class(base), pointer     :: b2(:)

   type(child), target      :: c1(2)
   class(child), pointer    :: c2(:)

   namelist /n1/ b1, c1
   namelist /n2/ b2, c2

   allocate ( b1(3), source = (/ child ( 'abc', 1001 ), child ( 'def', 1002 ), child ( 'ghi', 1003 ) /) )
   allocate ( b2(3), source = (/ child ( 'ABC', 2001 ), child ( 'DEF', 2002 ), child ( 'GHI', 2003 ) /) )
   c1= (/ child ( 'jkl', 3003 ), child ( 'mno', 3004 ) /)
   allocate ( c2(2), source = (/ child ( 'JKL', 4003 ), child ( 'MNO', 4004 ) /) )

   open ( 1, file = 'specific002a.1', form='formatted', access='sequential' )

   write ( 1, n1, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write ( 1, n2, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   rewind 1

   deallocate ( b1, b2, c2 )
   allocate ( child :: b1(3), b2(3), c2(2) )
   c1 = (/ child(), child() /)

   read ( 1, n1, iostat = stat, iomsg = msg )
   print *, stat, msg
   print *, b1, c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4

   read ( 1, n2, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 4_4

   select type ( b1 )
      type is ( child )
         if ( ( b1(1)%c /= 'abc' ) .or. ( b1(1)%i /= 1001 ) .or. &
              ( b1(2)%c /= 'def' ) .or. ( b1(2)%i /= 1002 ) .or. &
              ( b1(3)%c /= 'ghi' ) .or. ( b1(3)%i /= 1003 )      ) error stop 5_4
   end select

   select type ( b2 )
      type is ( child )
         if ( ( b2(1)%c /= 'ABC' ) .or. (b2(1)%i /= 2001 ) .or. &
              ( b2(2)%c /= 'DEF' ) .or. (b2(2)%i /= 2002 ) .or. &
              ( b2(3)%c /= 'GHI' ) .or. (b2(3)%i /= 2003 )      ) error stop 6_4
   end select

   if ( ( c1(1)%c /= 'jkl' ) .or. ( c1(1)%i /= 3003 ) .or. &
        ( c1(2)%c /= 'mno' ) .or. ( c1(2)%i /= 3004 )      ) error stop 7_4
   if ( ( c2(1)%c /= 'JKL' ) .or. ( c2(1)%i /= 4003 ) .or. &
        ( c2(2)%c /= 'MNO' ) .or. ( c2(2)%i /= 4004 )      ) error stop 8_4

end program
