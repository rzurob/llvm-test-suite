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
!*                                        Final Subroutine: Ensure DTIO can be invoked correctly during finalization with array (read)
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
      integer(4) :: i = -999
      contains
         final :: finalbase, finalbasearray
   end type

   type, extends(base) :: child
      character(3) :: c = 'xxx'
      contains
         final :: finalchildarray
   end type


   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base), intent(inout) :: dtv
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
         type(base), intent(inout) :: dtv
         read ( 1, "(DT'_finalizingbase'(6))", iostat = stat, iomsg = msg )      dtv
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
         print *, dtv%i
      end subroutine

      subroutine finalbasearray(dtv)
         type(base), intent(inout) :: dtv(:)
         character(100) :: fmt
         write( fmt, * ) "(", size(dtv),"(DT'finalizebasearray'(5)))"
         read ( 1, fmt, iostat = stat, iomsg = msg )                      dtv
         print *, dtv%i
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )                  error stop 2_4
      end subroutine

      subroutine finalchildarray(dtv)
         type(child), intent(inout) :: dtv(:)

         read ( 1, "(DT'_finalizingchild'(6,8))", iostat = stat, iomsg = msg )    dtv
         print *, dtv%i, dtv%c
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4
      end subroutine

end module

program final102
use m

   type(base)  :: b1(3)
   type(child) :: c1(3)
   type(base), allocatable :: b2(:)
   class(base), pointer    :: b3(:)

   open (1, file = 'final102.1', form='formatted', access='sequential' )

   b1 = (/ base(), base(), base() /)        !<- finalize b1, then base(1234), base(1235), base(1236) as scalars
   c1 = (/ child(), child(), child() /)     !<- finalize c1, then child(2234,'abc'), child(2235,'def'), child(2236,'ghi') as scalars
   allocate( b2(4), source = (/ base(), base(), base(), base() /) )
   allocate( b3(4), source = (/ child(), child(), child(), child() /) )

   deallocate( b2, b3 )

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt

   select type ( dtv )
      type is ( base )
         write ( fmt, "(A2,I1,A1)" ) '(I', v_list(1),')'
         read ( unit, fmt, iostat = iostat )    dtv%i
      type is ( child )
         write ( fmt, "(A2,I1,A2,I1,A1)" ) '(I', v_list(1),',A',v_list(2),')'
         read ( unit, fmt, iostat = iostat )    dtv%i, dtv%c
   end select
   iomsg = 'dtioread'

end subroutine
