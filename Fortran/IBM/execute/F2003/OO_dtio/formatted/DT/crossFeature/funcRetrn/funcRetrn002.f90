!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 21/03/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Function Return: Polymorphic Scalar Entities
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
      real(4) :: i = -999.0
      contains
         procedure, pass :: genCopy
   end type

   type, extends(base) :: child
      real(4) :: j = -999.0
   end type

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   integer :: stat
   character(150) :: msg

   contains

      class(base) function genCopy( dtv )
         class(base), intent(in) :: dtv
         allocatable :: genCopy
         allocate ( genCopy, source = dtv )
      end function

end module

program funcRetrn002
use m

   interface
      class(base) function genCopyExt( dtv )
         import base
         class(base), intent(in) :: dtv
         allocatable :: genCopyExt
      end function
   end interface

   class(base), allocatable  :: b1
   class(base), pointer      :: b2
   type(base)                :: b3 = base (100.0)

   class(child), allocatable :: c1
   class(child), pointer     :: c2
   type(child)               :: c3 = child(200.0,300.0)

   open (1, file = 'funcRetrn002.1', form='formatted', access='sequential' )

   allocate ( b1, source = base  ( 101.0 ) )
   allocate ( b2, source = child ( 102.0 , 103.0 ) )
   allocate ( c1, source = child ( 201.0 , 202.0 ) )
   allocate ( c2, source = child ( 203.0 , 204.0 ) )

   write ( 1, "(DT'_btbound'(7,2), DT'_ctbound'(8,3,9,4))", iostat = stat, iomsg = msg )  b1%genCopy(), b2%genCopy(), b3%genCopy(), c1%genCopy()
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write ( 1, "(DT'mod1'(7,1),DT'mod2'(8,2,9,3))", iostat = stat, iomsg = msg )           genCopy(b1), genCopy(c2)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   write ( 1, "(DT'Int1'(7,1,8,2),DT'Int2'(8,2,9,3))", iostat = stat, iomsg = msg )       genCopyInt(b2), genCopyInt(c3)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

   write ( 1, "(DT'Ext1'(7,1),DT'Ext2'(8,2,9,3))", iostat = stat, iomsg = msg )           genCopyExt(b3), genCopyExt(c1)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

   contains

      class(base) function genCopyInt( dtv )
         class(base), intent(in) :: dtv
         allocatable :: genCopyInt
         allocate ( genCopyInt, source = dtv )
      end function

end program

class(base) function genCopyExt( dtv )
   use m, only: base
   class(base), intent(in) :: dtv
   allocatable :: genCopyExt
   allocate ( genCopyExt, source = dtv )
end function

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt

   write ( unit, * ) ' iotype:', iotype, ' v_list:', v_list

   select type ( dtv )
      type is ( base )
         write ( fmt, "(A2,I1,A1,I1,A1)" ) '(F', v_list(1),'.',v_list(2),')'
         write ( unit, fmt, iostat = iostat )    dtv%i
      type is ( child )
         write ( fmt, "(A2,I1,A1,I1,A2,I1,A1,I1,A1)" ) '(F', v_list(1),'.',v_list(2),',F',v_list(3),'.',v_list(4),')'
         write ( unit, fmt, iostat = iostat )    dtv%i, dtv%j
   end select
   iomsg = 'dtiowrite'

end subroutine
