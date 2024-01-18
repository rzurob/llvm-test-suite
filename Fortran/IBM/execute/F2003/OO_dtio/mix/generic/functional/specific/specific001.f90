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
!*                                    - non-overridable specific type bound procedure
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
         procedure, non_overridable, pass :: write => writebase
         procedure, non_overridable, pass :: read => readbase
         generic :: write(formatted) => write
         generic :: read(formatted)  => read
   end type

   type, extends(base) :: child
      integer(4) :: i = -999
   end type

   contains

      subroutine writebase (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         select type ( dtv )
            type is ( base )
               write (unit, "(A3,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, 9999
            type is ( child )
               write (unit, "(A3,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
         end select

         iomsg = 'dtiowrite'

      end subroutine

      subroutine readbase (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         select type ( dtv )
            type is ( base )
               read (unit, "(A3,5X)" , iostat=iostat, iomsg=iomsg) dtv%c
            type is ( child )
               read (unit, "(A3,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
         end select

         iomsg = 'dtioread'

      end subroutine

end module

program specific001
   use m

   class(base), allocatable :: b1
   type(base) , pointer     :: b2

   class(child), allocatable :: c1
   type(child)               :: c2 = child ('jkl', 1003)

   integer :: stat
   character(200) :: msg

   allocate ( b1, source = child ( 'abc', 1001 ) )
   allocate ( b2, source = base  ( 'def' ) )
   allocate ( c1, source = child ( 'ghi', 1002 ) )

   open ( 1, file = 'specific001.1', form='formatted', access='sequential' )

   write ( 1, "(4(DT,:,/))", iostat = stat, iomsg = msg )    b1, b2, c1, c2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )     error stop 1_4

   rewind 1
   
   read ( 1, "(DT)", iostat = stat, iomsg = msg )    c2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )      error stop 2_4

   read ( 1, "(DT)", iostat = stat, iomsg = msg )    c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )      error stop 3_4
   
   read ( 1, "(DT,/,DT)", iostat = stat, iomsg = msg ) b2, b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )      error stop 4_4

   select type ( b1 )
      type is (child)
        if ( ( b1%c /= 'jkl' ) .or. ( b1%i /= 1003 ) )  error stop 5_4
   end select

   if ( b2%c /= 'ghi' )                                 error stop 6_4
   if ( ( c1%c /= 'def' ) .or. ( c1%i /= 9999 ) )       error stop 7_4
   if ( ( c2%c /= 'abc' ) .or. ( c2%i /= 1001 ) )       error stop 8_4

   close ( 1, status ='delete')

end program
