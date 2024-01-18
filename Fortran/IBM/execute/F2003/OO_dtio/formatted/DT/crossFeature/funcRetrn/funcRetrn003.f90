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
!*                                        Function Return: Non Polymorphic Array Entities
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
         procedure, pass :: genArray
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

      type(base) function genArray(dtv)
         class(base), intent(in) :: dtv
         dimension :: genArray(4)
         genArray = (/ base(100.0), base(101.0), base(102.0), base(103.0) /)
      end function

end module

program funcRetrn003
use m

   interface
      type(child) function genArrayExt()
         import child
         allocatable :: genArrayExt(:)
      end function
   end interface

   type(base) :: dummy

   open (1, file = 'funcRetrn003.1', form='formatted', access='sequential' )

   write ( 1, "(DT'_btbound'(7,2), DT'_btbound'(8,3))", iostat = stat, iomsg = msg )      dummy%genArray()
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write ( 1, "(DT'mod1'(7,1),DT'mod2'(8,2),DT'mod3'(9,3),DT'mod4'(9,4))", iostat = stat, iomsg = msg )       genArray(dummy)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   write ( 1, "(DT'Int1'(7,1),DT'Int2'(8,2))", iostat = stat, iomsg = msg )               genArrayInt()
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

   write ( 1, "(DT'Ext1'(7,1,8,2),DT'Ext2'(8,2,9,3),DT'Ext3'(9,3,9,4))", iostat = stat, iomsg = msg )           genArrayExt()
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 4_4

   contains

      type(base) function genArrayInt()
         pointer :: genArrayInt(:,:)
         allocate ( genArrayInt(2,2), source = reshape ( source = (/ base(301.0), base(302.0), base(303.0), base(304.0) /), shape = (/2,2/)))
      end function

end program

type(child) function genArrayExt( )
   use m, only: child
   allocatable :: genArrayExt(:)
   allocate ( genArrayExt(3), source = (/ child(201.0, 202.0), child(203.0,204.0), child(205.0,206.0) /) )
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
