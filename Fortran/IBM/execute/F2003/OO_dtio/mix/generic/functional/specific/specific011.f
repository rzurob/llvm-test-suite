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
!*                                    - deferred specific type bound procedure
!*                                         - deferred binding in parent type, and child
!*                                           type has non-overridable type bound in module/external procedure
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
         procedure, non_overridable, pass :: write
         procedure, non_overridable, pass :: read
   end type

   type, extends(child) :: gen3
      integer(4) :: j = -999
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

   contains

      subroutine read (dtv, unit, iostat, iomsg)
         class(child), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         select type ( dtv )
            type is ( child )
               read (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
            type is ( gen3 )
               read (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i, dtv%j
         end select

         iomsg = 'dtioreadc'

      end subroutine

end module

program specific011
   use m

   integer(4) :: stat
   character(200) :: msg

   class(base), allocatable  :: b1, b2
   class(child), pointer     :: c1, c2
   class(gen3), allocatable  :: g1

   allocate ( b1, source = child ( 'abc', 1001 ) )
   allocate ( b2, source = gen3 ( 'def', 1002, 1003 ) )
   allocate ( c1, source = child ( 'ghi', 1004 ) )
   allocate ( c2, source = gen3 ( 'jkl', 1005, 1006 ) )
   allocate ( g1, source = gen3 ( 'mno', 1007, 1008 ) )

   open ( 1, file = 'specific011.1', form='unformatted', access='sequential' )

   write ( 1, iostat = stat, iomsg = msg ) b1, b2, c1, c2, g1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 1_4

   rewind 1

   deallocate ( b1, b2, c1, c2, g1 )
   allocate ( child :: b1, c1 )
   allocate ( gen3  :: b2, c2, g1 )

   read ( 1, iostat = stat, iomsg = msg )  b1, b2, c1, c2, g1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) ) error stop 2_4

   select type ( b1 )
      type is ( child )
         if ( ( b1%c /= 'abc' ) .or. ( b1%i /= 1001 ) .or. ( c1%c /= 'ghi' ) .or. ( c1%i /= 1004 ) ) error stop 3_4
   end select
   
   select type ( b2 )
      type is ( gen3 )
         select type ( c2 ) 
            type is ( gen3 )
               if ( ( b2%c /= 'def' ) .or. ( b2%i /= 1002 ) .or. ( b2%j /= 1003 ) .or. &
                    ( c2%c /= 'jkl' ) .or. ( c2%i /= 1005 ) .or. ( c2%j /= 1006 ) .or. &
                    ( g1%c /= 'mno' ) .or. ( g1%i /= 1007 ) .or. ( g1%j /= 1008 ) ) error stop 4_4
         end select
   end select

   close (1, status = 'delete' )

end program

subroutine write (dtv, unit, iostat, iomsg)
   use m, only: child, gen3
   class(child), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type ( dtv )
      type is ( child )
         write (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
      type is ( gen3 )
         write (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i, dtv%j
   end select

   iomsg = 'dtiowritec'

end subroutine
