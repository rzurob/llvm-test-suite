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
!*                                    - specific binding available both in parent and extended types
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

   type, extends(base) :: child
      integer(4) :: i = -999
      contains
         procedure, pass :: write => writechild
         procedure, pass :: read => readchild
   end type

   type, extends(child) :: gen3
      integer(4) :: j = -999
   end type

   contains

      subroutine writebase (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write ( unit, *, iostat = iostat , iomsg = iomsg ) dtv%c
         iomsg = 'dtiobasewrite'
      end subroutine

      subroutine readbase (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read ( unit, "(1X,A3)", iostat = iostat , iomsg = iomsg ) dtv%c
         iomsg = 'dtiobaseread'
      end subroutine

      subroutine writechild (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         select type ( dtv )
            type is ( child )
               write (unit, *, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
               iomsg = 'dtiochildwrite'
            type is ( gen3 )
               write (unit, *, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i, dtv%j
               iomsg = 'dtiogen3write'
         end select

      end subroutine

      subroutine readchild (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         select type ( dtv )
            type is ( child )
               read (unit, "(1X,A3,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
               iomsg = 'dtiochildread'
            type is ( gen3 )
               read (unit, "(1X,A3,1X,I4,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i, dtv%j
               iomsg = 'dtiogen3read'
         end select

      end subroutine

end module

program specific004
   use m

   class(base), allocatable :: b1
   class(child), pointer    :: c1
   class(gen3), pointer     :: g1

   integer :: stat
   character(200) :: msg

   allocate ( b1, source = base  ( 'abc' ) )
   allocate ( c1, source = child ( 'def', 2001 ) )
   allocate ( g1, source = gen3  ( 'ghi', 3001, 3002 ) )

   open ( 1, file = 'specific004.1', form='formatted', access='sequential' )

   write ( 1, "(DT)", iostat = stat, iomsg = msg )  b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiobasewrite' ) )  error stop 1_4
   write ( 1, "(DT)", iostat = stat, iomsg = msg )  c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiochildwrite' ) ) error stop 2_4
   write ( 1, "(DT)", iostat = stat, iomsg = msg )  g1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiogen3write' ) )  error stop 3_4

   deallocate ( b1, c1 )

   allocate ( b1, source = child ( 'jkl', 2002 ) )
   allocate ( c1, source = gen3  ( 'mno', 3003, 3004 ) )

   write ( 1, "(DT)", iostat = stat, iomsg = msg )  b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiochildwrite' ) ) error stop 4_4
   write ( 1, "(DT)", iostat = stat, iomsg = msg )  c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiogen3write' ) )  error stop 5_4

   deallocate ( b1 )

   allocate ( b1, source = gen3 ( 'pqr', 3005, 3006 ) )
   write ( 1, "(DT)", iostat = stat, iomsg = msg )  b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiogen3write' ) )  error stop 6_4

   deallocate ( b1, c1, g1 )
   allocate   ( b1, c1, g1 )

   rewind 1

   read ( 1, "(DT)", iostat = stat, iomsg = msg )  b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiobaseread' ) )  error stop 7_4
   read ( 1, "(DT)", iostat = stat, iomsg = msg )  c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiochildread' ) ) error stop 8_4
   read ( 1, "(DT)", iostat = stat, iomsg = msg )  g1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiogen3read' ) )  error stop 9_4

   if ( b1%c /= 'abc' ) error stop 10_4
   if ( ( c1%c /= 'def' ) .or. ( c1%i /= 2001 ) ) error stop 11_4
   if ( ( g1%c /= 'ghi' ) .or. ( g1%i /= 3001 ) .or. ( g1%j /= 3002 ) )  error stop 12_4

   deallocate ( b1, c1 )

   allocate ( b1, source = child () )
   allocate ( c1, source = gen3  () )

   read ( 1, "(DT)", iostat = stat, iomsg = msg )  b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiochildread' ) ) error stop 13_4
   read ( 1, "(DT)", iostat = stat, iomsg = msg )  c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiogen3read' ) )  error stop 14_4

   select type ( b1 )
      type is ( child )
         if ( ( b1%c /= 'jkl' ) .or. ( b1%i /= 2002 ) ) error stop 15_4
   end select

   select type ( c1 )
      type is ( gen3 )
         if ( ( c1%c /= 'mno' ) .or. ( c1%i /= 3003 ) .or. ( c1%j /= 3004 ) )  error stop 16_4
   end select

   deallocate ( b1 )

   allocate ( b1, source = gen3 () )
   read ( 1, "(DT)", iostat = stat, iomsg = msg )  b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiogen3read' ) )  error stop 17_4

   select type ( b1 )
      type is ( gen3 )
         if ( ( b1%c /= 'pqr' ) .or. ( b1%i /= 3005 ) .or. ( b1%j /= 3006 ) )  error stop 18_4
   end select

   close ( 1, status ='delete')

end program
