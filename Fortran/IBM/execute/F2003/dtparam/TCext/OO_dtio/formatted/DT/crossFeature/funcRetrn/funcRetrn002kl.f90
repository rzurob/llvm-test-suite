!*  ===================================================================
!*
!*  TEST CASE NAME             : funcRetrn002kl
!*
!*  DATE                       : 2007-06-06 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
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

   type base (kb)
      integer, kind :: kb
      real(kb) :: i = -999.0
      contains
         procedure, pass :: genCopy
   end type

   type, extends(base) :: child (kc)
      integer, kind :: kc
      real(kc) :: j = -999.0
   end type

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(in) :: dtv
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

      class(base(4)) function genCopy( dtv )
         class(base(4)), intent(in) :: dtv
         allocatable :: genCopy
         allocate ( genCopy, source = dtv )
      end function

end module

program funcRetrn002kl
use m

   interface
      class(base(4)) function genCopyExt( dtv )
         import base
         class(base(4)), intent(in) :: dtv
         allocatable :: genCopyExt
      end function
   end interface

   class(base(4)), allocatable  :: b1
   class(base(4)), pointer      :: b2
   type(base(4))                :: b3 = base(4) (100.0)

   class(child(4,4)), allocatable :: c1
   class(child(4,4)), pointer     :: c2
   type(child(4,4))               :: c3 = child(4,4)(200.0,300.0)

   open (1, file = 'funcRetrn002kl.1', form='formatted', access='sequential' )

   allocate ( b1, source = base(4)  ( 101.0 ) )
   allocate ( b2, source = child(4,4) ( 102.0 , 103.0 ) )
   allocate ( c1, source = child(4,4) ( 201.0 , 202.0 ) )
   allocate ( c2, source = child(4,4) ( 203.0 , 204.0 ) )

   write ( 1, "(DT'_btbound'(7,2), DT'_ctbound'(8,3,9,4))", iostat = stat, iomsg = msg )  b1%genCopy(), b2%genCopy(), b3%genCopy(), c1%genCopy()
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write ( 1, "(DT'mod1'(7,1),DT'mod2'(8,2,9,3))", iostat = stat, iomsg = msg )           genCopy(b1), genCopy(c2)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   write ( 1, "(DT'Int1'(7,1,8,2),DT'Int2'(8,2,9,3))", iostat = stat, iomsg = msg )       genCopyInt(b2), genCopyInt(c3)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

   write ( 1, "(DT'Ext1'(7,1),DT'Ext2'(8,2,9,3))", iostat = stat, iomsg = msg )           genCopyExt(b3), genCopyExt(c1)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

   contains

      class(base(4)) function genCopyInt( dtv )
         class(base(4)), intent(in) :: dtv
         allocatable :: genCopyInt
         allocate ( genCopyInt, source = dtv )
      end function

end program

class(base(4)) function genCopyExt( dtv )
   use m, only: base
   class(base(4)), intent(in) :: dtv
   allocatable :: genCopyExt
   allocate ( genCopyExt, source = dtv )
end function

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child

   class(base(4)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt

   write ( unit, * ) ' iotype:', iotype, ' v_list:', v_list

   select type ( dtv )
      type is ( base(4) )
         write ( fmt, "(A2,I1,A1,I1,A1)" ) '(F', v_list(1),'.',v_list(2),')'
         write ( unit, fmt, iostat = iostat )    dtv%i
      type is ( child(4,4) )
         write ( fmt, "(A2,I1,A1,I1,A2,I1,A1,I1,A1)" ) '(F', v_list(1),'.',v_list(2),',F',v_list(3),'.',v_list(4),')'
         write ( unit, fmt, iostat = iostat )    dtv%i, dtv%j
   end select
   iomsg = 'dtiowrite'

end subroutine
