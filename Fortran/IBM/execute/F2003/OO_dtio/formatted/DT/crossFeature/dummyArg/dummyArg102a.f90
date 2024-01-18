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
!*                                        Argument Association: (non-) polymorphic scalar dummy
!*                                        argument with pointer/allocatable attribute (read)
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
   end type

   type, extends(base) :: child
      integer(4) :: j = -999
   end type

   type, extends(child) :: gen3
      integer(4) :: k = -999
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
   character(20) :: rbuffer(5)
   integer :: idx

   contains

      subroutine foo ( dtv )
         class(base), intent(inout), allocatable :: dtv
         character(17) :: fmt = "(DT'_foo'(5,6,7))"

         read (1, fmt, iostat = stat, iomsg = msg ) dtv
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )  error stop 1_4

      end subroutine

      subroutine bar ( dtv )
         class(child), intent(out), pointer :: dtv
      10 format (DT'_bar'(8,9,10))

         read (1, 10, iostat = stat, iomsg = msg ) dtv

         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )  error stop 2_4

      end subroutine

end module

program dummyArg102a
use m

   class(base), allocatable  :: b1
   class(child), pointer     :: c1

   open (1, file = 'dummyArg102a.1', form='formatted', access='sequential' )

   allocate ( b1 )
   allocate ( c1 )
   idx = 1

   call foo ( b1 )
   call bar ( c1 )

   if ( b1%i /= 101 ) error stop 3_4
   if ( ( c1%i /= 201 ) .or. ( c1%j /= 202 ) ) error stop 4_4

   deallocate ( b1 , c1 )
   allocate ( child :: b1 )
   allocate ( gen3 :: c1 )

   call foo ( b1 )
   call bar ( c1 )

   select type ( b1 )
      type is ( child )
         if ( ( b1%i /= 102 ) .or. ( b1%j /= 103 ) ) error stop 5_4
   end select

   select type ( c1 )
      type is ( gen3 )
         if ( ( c1%i /= 203 ) .or. ( c1%j /= 204 ) .or. ( c1%k /= 205 ) ) error stop 6_4
   end select

   deallocate ( b1 )
   allocate ( gen3 :: b1 )

   call foo ( b1 )

   select type ( b1 )
      type is ( gen3 )
         if ( ( b1%i /= 104 ) .or. ( b1%j /= 105 ) .or. ( b1%k /= 106 ) ) error stop 7_4
   end select

   print *, rbuffer

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child, gen3, rbuffer, idx

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt
   write ( rbuffer(idx), * ) iotype, v_list
   idx = idx + 1
   select type ( dtv )
      type is ( base )
         write ( fmt, * ) '(I', v_list(1),')'
         read ( unit, fmt, iostat = iostat )    dtv%i
      type is ( child )
         write ( fmt, * ) '(I', v_list(1),', I', v_list(2),')'
         read ( unit, fmt, iostat = iostat )    dtv%i, dtv%j
      type is ( gen3 )
         write ( fmt, * ) '(I', v_list(1),', I', v_list(2),', I',v_list(3),')'
         read ( unit, fmt, iostat = iostat )    dtv%i, dtv%j, dtv%k
   end select
   iomsg = 'dtioread'

end subroutine
