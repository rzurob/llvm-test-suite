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
!*                                    - specific binding is a overriding binding
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
   end type

   type, extends(base) :: child
      integer(4) :: i = -999
      contains
         procedure, pass :: write => writechild
         procedure, pass :: read => readchild
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

         !  no implementation

      end subroutine

      subroutine readbase (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         !  no implementation

      end subroutine

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

         read (unit, "(A3,1X,I4)" , iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
         iomsg = 'dtioread'
      end subroutine

end module

program specific003
   use m

   class(base), allocatable :: b1
   class(base), pointer     :: b2

   class(child), allocatable :: c1
   type(child)               :: c2 = child ('jkl', 1004)

   integer :: stat
   character(200) :: msg

   allocate ( b1, source = child ( 'abc', 1001 ) )
   allocate ( b2, source = child ( 'def', 1002 ) )
   allocate ( c1, source = child ( 'ghi', 1003 ) )

   open ( 1, file = 'specific003.1', form='formatted', access='sequential' )

   select type ( g => b1 )
      class is (child)
         write ( 1, "(DT)", iostat = stat, iomsg = msg )    g
   end select
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4
   
   select type ( b2 )
      type is (child)
         write ( 1, "(DT)", iostat = stat, iomsg = msg )    b2
   end select
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4
   
   write ( 1, "(2(DT,:,/))", iostat = stat, iomsg = msg )    c1, c2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

   rewind 1

   select type ( b1 )
      type is ( child )
         select type ( b2 )
            class is ( child )
               read ( 1, "(DT)", iostat = stat, iomsg = msg )  c2, c1, b2, b1
         end select
   end select
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )      error stop 4_4
   
   select type ( b1 )
      type is (child)
        if ( ( b1%c /= 'jkl' ) .or. ( b1%i /= 1004 ) )  error stop 5_4
   end select

   select type ( b2 )
      type is (child)
        if ( ( b2%c /= 'ghi' ) .or. ( b2%i /= 1003 ) )  error stop 6_4
   end select
   if ( ( c1%c /= 'def' ) .or. ( c1%i /= 1002 ) )       error stop 7_4
   if ( ( c2%c /= 'abc' ) .or. ( c2%i /= 1001 ) )       error stop 8_4

   close ( 1, status ='delete')

end program
