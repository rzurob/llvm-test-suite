!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : funcRetrn001kl
!*
!*  PROGRAMMER                 : David Forster (derived from funcRetrn001 by Robert Ma)
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

   type base (kb)
      integer, kind :: kb
      real(kb) :: i = -999.0
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

      type(base(4)) function genCopy( dtv )
         type(base(4)), intent(in) :: dtv
         allocatable :: genCopy
         allocate ( genCopy, source = dtv )
      end function

end module

program funcRetrn001kl
use m

   type(base(4)), allocatable  :: b1
   type(base(4)), pointer      :: b2
   type(base(4))                :: b3 = base(4) (103.0)

   type(child(4,4)), allocatable :: c1
   type(child(4,4)), pointer     :: c2
   type(child(4,4))              :: c3 = child(4,4)(205.0,206.0)
   
   procedure(type(child(4,4))) :: genCopyExt

   open (1, file = 'funcRetrn001kl.1', form='formatted', access='sequential' )

   allocate ( b1, source = base(4)  ( 101.0 ) )
   allocate ( b2, source = base(4)  ( 102.0 ) )
   allocate ( c1, source = child(4,4) ( 201.0 , 202.0 ) )
   allocate ( c2, source = child(4,4) ( 203.0 , 204.0 ) )

   write ( 1, "(DT'mod1'(7,1),DT'mod2'(8,2),DT'mod3'(9,3))", iostat = stat, iomsg = msg ) genCopy(b1), genCopy(b2), genCopy(b3)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write ( 1, "(DT'Int1'(7,1),DT'Int2'(8,2))", iostat = stat, iomsg = msg )               genCopyInt(b1), genCopyInt(b2), genCopyInt(b3)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   write ( 1, "(DT'Ext1'(7,1,7,2))", iostat = stat, iomsg = msg )                         genCopyExt(c1), genCopyExt(c2), genCopyExt(c3)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

   contains

      type(base(4)) function genCopyInt( dtv )
         type(base(4)), intent(in) :: dtv
         genCopyInt = dtv
      end function

end program

type(child(4,4)) function genCopyExt( dtv )
   use m, only: child
   type(child(4,4)), intent(in) :: dtv
   genCopyExt = dtv
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
