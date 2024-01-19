!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Function Return: Non Polymorphic Scalar Entities
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

      type(base) function genCopy( dtv )
         type(base), intent(in) :: dtv
         allocatable :: genCopy
         allocate ( genCopy, source = dtv )
      end function

end module

program funcRetrn001
use m

   type(base), allocatable  :: b1
   type(base), pointer      :: b2
   type(base)                :: b3 = base (103.0)

   type(child), allocatable :: c1
   type(child), pointer     :: c2
   type(child)              :: c3 = child(205.0,206.0)

   procedure(type(child)) :: genCopyExt

   open (1, file = 'funcRetrn001.1', form='formatted', access='sequential' )

   allocate ( b1, source = base  ( 101.0 ) )
   allocate ( b2, source = base  ( 102.0 ) )
   allocate ( c1, source = child ( 201.0 , 202.0 ) )
   allocate ( c2, source = child ( 203.0 , 204.0 ) )

   write ( 1, "(DT'mod1'(7,1),DT'mod2'(8,2),DT'mod3'(9,3))", iostat = stat, iomsg = msg ) genCopy(b1), genCopy(b2), genCopy(b3)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write ( 1, "(DT'Int1'(7,1),DT'Int2'(8,2))", iostat = stat, iomsg = msg )               genCopyInt(b1), genCopyInt(b2), genCopyInt(b3)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   write ( 1, "(DT'Ext1'(7,1,7,2))", iostat = stat, iomsg = msg )                         genCopyExt(c1), genCopyExt(c2), genCopyExt(c3)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

   contains

      type(base) function genCopyInt( dtv )
         type(base), intent(in) :: dtv
         genCopyInt = dtv
      end function

end program

type(child) function genCopyExt( dtv )
   use m, only: child
   type(child), intent(in) :: dtv
   genCopyExt = dtv
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
