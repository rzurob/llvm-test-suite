!*  ===================================================================
!*
!*  TEST CASE NAME             : final101kl
!*
!*  DATE                       : 2007-06-06 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Final Subroutine: Ensure DTIO can be invoked correctly during finalization (read)
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
      contains
         final :: finalbase
   end type

   type, extends(base) :: child (lc)
      integer, len :: lc
      character(lc) :: c = 'xxx'
      contains
         final :: finalchild
   end type


   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(inout) :: dtv
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

      subroutine finalbase(dtv)
         type(base(4)), intent(inout) :: dtv
         read ( 1, "(DT'_finalizingbase'(6))", iostat = stat, iomsg = msg )      dtv
         print *, 'inside finalbase'
         print *, dtv%i
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
      end subroutine

      subroutine finalchild(dtv)
         type(child(4,*)), intent(inout) :: dtv
         read ( 1, "(DT'_finalizingchild'(6,8))", iostat = stat, iomsg = msg )    dtv
         print *, 'inside finalchild'
         print *, dtv%i, dtv%c
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4
      end subroutine

end module

program final101kl
use m

   type(base(4))  :: b1
   type(child(4,3)) :: c1
   type(base(4)), allocatable :: b2
   class(base(4)), pointer    :: b3

   open (1, file = 'final101kl.1', form='formatted', access='sequential' )

   b1 = base(4)()         !<- finalize b1, then base(1234)
   c1 = child(4,3)()  !<- finalize c1, then child(5678,'abc')

   allocate ( b2, source = base(4)() )        !<- finalize base(2345)
   allocate ( b3, source = child(4,3)() ) !<- finalize child(3456,'def')

   deallocate( b2, b3 )  !<- finalize b2, b3

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child

   class(base(4)), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt

   select type ( dtv )
      type is ( base(4) )
         write ( fmt, "(A2,I1,A1)" ) '(I', v_list(1),')'
         read ( unit, fmt, iostat = iostat )    dtv%i
      type is ( child(4,*) )
         write ( fmt, "(A2,I1,A2,I1,A1)" ) '(I', v_list(1),',A',v_list(2),')'
         read ( unit, fmt, iostat = iostat )    dtv%i, dtv%c
   end select
   iomsg = 'dtioread'

end subroutine
