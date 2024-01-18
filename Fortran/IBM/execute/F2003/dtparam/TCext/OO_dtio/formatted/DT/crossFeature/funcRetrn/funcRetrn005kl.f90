!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : funcRetrn005kl
!*
!*  PROGRAMMER                 : David Forster (derived from funcRetrn005 by Robert Ma)
!*  DATE                       : 2007-06-06 (original: 21/03/2005)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Function Return: Unlimited Polymorphic Scalar/Array Entities
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

      class(*) function genCopy( dtv )
         class(base(4)), intent(in) :: dtv
         pointer :: genCopy
         allocate ( genCopy, source = dtv )
      end function

end module

program funcRetrn005kl
use m

   interface
      class(*) function genCopyExt( dtv )
         import base
         class(base(4)), intent(in) :: dtv
         allocatable :: genCopyExt(:)
      end function
   end interface

   class(base(4)), allocatable  :: b1
   class(base(4)), pointer      :: b2
   type(base(4))                :: b3 = base(4) (100.0)

   open (1, file = 'funcRetrn005kl.1', form='formatted', access='sequential' )

   allocate ( b1, source = base(4)  ( 101.0 ) )
   allocate ( b2, source = child(4,4) ( 102.0 , 103.0 ) )

   select type ( g => b1%genCopy() )
      class is ( base(4) )
         write ( 1, "(DT'_btbound'(7,2))", iostat = stat, iomsg = msg )  g
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4
   end select
   
   select type ( g => b2%genCopy() )
      class is ( base(4) )
         write ( 1, "(DT'_ctbound'(7,2,8,3))", iostat = stat, iomsg = msg )  g
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4
   end select
   
   select type ( g => genCopy(b3) )
      class is ( base(4) )
         write ( 1, "(DT'mod1'(7,1),DT'mod2'(8,2,9,3))", iostat = stat, iomsg = msg )       g
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4
   end select

   select type ( g => genCopyInt(b2) )
      class is ( base(4) )
         write ( 1, "(DT'Int1'(7,1,8,2),DT'Int2'(8,2,9,3))", iostat = stat, iomsg = msg )   g
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 4_4
   end select

   select type ( g => genCopyExt(b3) )
      class is ( base(4) )
         write ( 1, "(DT'Ext1'(7,1),DT'Ext2'(8,2))", iostat = stat, iomsg = msg )           g
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 5_4
   end select

   contains

      class(base(4)) function genCopyInt( dtv )
         class(base(4)), intent(in) :: dtv
         pointer :: genCopyInt(:,:)
         allocate ( genCopyInt(2,2), source = reshape( source = (/ dtv, dtv, dtv, dtv /), shape = (/2,2/) ) )
      end function

end program

class(*) function genCopyExt( dtv )
   use m, only: base
   class(base(4)), intent(in) :: dtv
   allocatable :: genCopyExt(:)
   allocate ( genCopyExt(3), source = (/ dtv, dtv, dtv /) )
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
