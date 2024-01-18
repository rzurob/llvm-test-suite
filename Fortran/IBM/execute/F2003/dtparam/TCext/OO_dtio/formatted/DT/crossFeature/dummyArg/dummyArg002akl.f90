!*  ===================================================================
!*
!*  DATE                       : 2007-06-06 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Argument Association: (non-) polymorphic scalar dummy
!*                                        argument with pointer/allocatable attribute
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
      integer(kb) :: i = -999
   end type

   type, extends(base) :: child (kc)
      integer, kind :: kc
      integer(kc) :: j = -999
   end type

   type, extends(child) :: gen3 (kg)
      integer, kind :: kg
      integer(kg) :: k = -999
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

      subroutine foo ( dtv )
         class(base(4)), intent(in), allocatable :: dtv
         character(17) :: fmt = "(DT'_foo'(5,6,7))"

         write (1, fmt, iostat = stat, iomsg = msg ) dtv
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )  error stop 1_4

      end subroutine

      subroutine bar ( dtv )
         class(child(4,4)), intent(inout), pointer :: dtv
      10 format (DT'_bar'(8,9,10))

         write (1, 10, iostat = stat, iomsg = msg ) dtv

         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )  error stop 2_4

      end subroutine

end module

program dummyArg002akl
use m

   class(base(4)), allocatable  :: b1
   class(child(4,4)), pointer     :: c1

   open (1, file = 'dummyArg002akl.1', form='formatted', access='sequential' )

   allocate ( b1, source = base(4)(101) )
   allocate ( c1, source = child(4,4)(201, 202) )

   call foo ( b1 )
   call bar ( c1 )

   deallocate ( b1 , c1 )
   allocate ( b1, source = child(4,4) ( 102, 103 ) )
   allocate ( c1, source = gen3(4,4,4)  ( 203, 204, 205 ) )

   call foo ( b1 )
   call bar ( c1 )

   deallocate ( b1 )
   allocate ( b1, source = gen3(4,4,4) ( 104, 105, 106 ) )

   call foo ( b1 )

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child, gen3

   class(base(4)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt

   select type ( dtv )
      type is ( base(4) )
         write ( unit, * ) ' iotype:', iotype, ' v_list:', v_list(1)
         write ( fmt, * ) '(I', v_list(1),')'
         write ( unit, fmt, iostat = iostat )    dtv%i
      type is ( child(4,4) )
         write ( unit, * ) ' iotype:', iotype, ' v_list:', v_list(1:2)
         write ( fmt, * ) '(I', v_list(1),', I', v_list(2),')'
         write ( unit, fmt, iostat = iostat )    dtv%i, dtv%j
      type is ( gen3(4,4,4) )
         write ( unit, * ) ' iotype:', iotype, ' v_list:', v_list(1:3)
         write ( fmt, * ) '(I', v_list(1),', I', v_list(2),', I',v_list(3),')'
         write ( unit, fmt, iostat = iostat )    dtv%i, dtv%j, dtv%k
   end select
   iomsg = 'dtiowrite'

end subroutine
