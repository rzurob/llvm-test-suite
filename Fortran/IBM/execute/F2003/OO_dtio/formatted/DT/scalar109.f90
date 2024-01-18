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
!*                                        Scalar unlimited polymorphic entity contains
!*                                        unlimited polymorphic component within select type (read)
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
      integer(4) :: i
      class(*), pointer :: u => null()
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

   integer :: idx
   character(20) :: rbuffer(4)

end module

program scalar109
use m

   class(*), target, allocatable :: u1
   class(*), pointer             :: u2

   integer, target :: i1 = -999
   character(3), target :: c1 = 'xxx'

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT'_u1'(1))"

   open (1, file = 'scalar109.1', form='formatted', access='sequential' )

   allocate( u1, source = base(-999,null()))
   allocate( u2, source = base(-999,null()))

10 format (DT'_u2'(2,3))
   idx = 1

   select type ( u1 )
      type is ( base )
         u1%u => i1
         read ( 1, fmt, iostat = stat, iomsg = msg )                          u1
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )   error stop 1_4

         select type ( d => u1%u )
            type is ( integer )
               if ( ( u1%i /= 456 ) .or. ( d /= 123 ) .or. ( i1 /= 123 ) )    error stop 2_4
         end select

         u1%u => c1
         read ( 1, "(DT'_u1'(2))", iostat = stat, iomsg = msg )               u1
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )   error stop 3_4

         select type ( d => u1%u )
            type is ( character(*) )
               if ( ( u1%i /= 789 ) .or. ( d /= 'GHI' ) .or. ( c1 /= 'GHI' ) )    error stop 4_4
         end select

         u1%u => u2
         read ( 1, "(DT'_u1'(3,4))", iostat = stat, iomsg = msg )             u1
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )   error stop 5_4

         select type ( d => u1%u )
            type is ( base )
               select type ( u2 )
                  type is ( base )
                     if ( ( u1%i /= 567 ) .or. ( d%i /= 234 ) .or. ( u2%i /= 234 ) )    error stop 6_4
               end select
         end select

         deallocate ( u1%u )
         read ( 1, "(DT'_u1'(5,6,7))", iostat = stat, iomsg = msg )           u1
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )   error stop 7_4

         if ( ( u1%i /= 890 )  )    error stop 8_4

   end select

   print *, rbuffer

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, rbuffer, idx

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write ( rbuffer(idx), * ) iotype, v_list
   idx = idx + 1

   if ( associated(dtv%u) ) then
      select type ( g => dtv%u )
         type is ( integer )
            read ( unit, "(/I4)" ) g
         type is ( character(*) )
            read ( unit, "(/A4)" ) g
         type is ( base )
            read ( unit, "(/I4)" ) g%i
      end select
   end if

   read ( unit, "(/I4)" ) dtv%i

   iomsg = 'dtioread'

end subroutine
