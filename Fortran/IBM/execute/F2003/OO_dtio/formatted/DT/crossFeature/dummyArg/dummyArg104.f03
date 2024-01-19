!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Argument Association: assumed shape array dummy argument (read)
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
   character(20) :: rbuffer(28)
   integer :: idx
   contains

      subroutine foo ( dtv )
         type(base), intent(inout) :: dtv(:)
         character(63) :: fmt = "(DT'_type_base1'(5),/,DT'_type_base2'(6),/, DT'_type_base3'(7))"

         read (1, fmt, iostat = stat, iomsg = msg ) dtv
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )  error stop 1_4

      end subroutine

      subroutine bar ( dtv )
         class(base), intent(out) :: dtv(11:)
      10 format (DT'_class_base11'(4,5),/,DT'_class_base12'(5,6),/,DT'_class_base13'(6,7))

         read (1, 10, iostat = stat, iomsg = msg ) dtv

         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )  error stop 2_4

      end subroutine

end module

program dummyArg104
use m

   type(base), allocatable  :: b1(:)
   type(base)               :: b2(3)
   class(base), pointer     :: b3(:)

   type(child), allocatable :: c1(:)
   type(child)              :: c2(3)
   class(child), pointer    :: c3(:)

   open (1, file = 'dummyArg104.1', form='formatted', access='sequential' )

   allocate ( b1(2) )
   allocate ( child :: b3(4) )

   allocate ( c1(4) )
   allocate ( c3(4:6) )

   idx = 1

   call foo ( b1 )
   call foo ( b2 )
   call foo ( b3 )

   print *, b1%i
   print *, b2%i

   select type ( b3 )
      type is ( child )
         print *, b3%i  !<- only b3%i will be read in the call to foo()
         print *, b3%j
   end select

   call bar ( b1 )
   call bar ( b2 )
   call bar ( b3 )

   print *, b1%i
   print *, b2%i
   select type ( b3 )
      type is ( child )
         print *, b3%i
         print *, b3%j
   end select

   call bar ( dtv = c1 )
   call bar ( c2 )
   call bar ( c3 )

   print *, c1%i
   print *, c1%j
   print *, c2%i
   print *, c2%j
   print *, c3%i
   print *, c3%j

   print *, rbuffer
end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child, rbuffer, idx

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
   end select
   iomsg = 'dtioread'

end subroutine
