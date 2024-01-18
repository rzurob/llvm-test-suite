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

   type base
      integer(4) :: i = -999
   end type

   type, extends(base) :: child
      integer(4) :: j = -999
   end type

   type, extends(child) :: gen3
      integer(4) :: k = -999
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

      subroutine foo ( dtv )
         class(base), intent(in), allocatable :: dtv
         character(17) :: fmt = "(DT'_foo'(5,6,7))"

         write (1, fmt, iostat = stat, iomsg = msg ) dtv
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )  error stop 1_4

      end subroutine

      subroutine bar ( dtv )
         class(child), intent(inout), pointer :: dtv
      10 format (DT'_bar'(8,9,10))

         write (1, 10, iostat = stat, iomsg = msg ) dtv

         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )  error stop 2_4

      end subroutine

end module

program dummyArg002a
use m

   class(base), allocatable  :: b1
   class(child), pointer     :: c1

   open (1, file = 'dummyArg002a.1', form='formatted', access='sequential' )

   allocate ( b1, source = base(101) )
   allocate ( c1, source = child(201, 202) )

   call foo ( b1 )
   call bar ( c1 )

   deallocate ( b1 , c1 )
   allocate ( b1, source = child ( 102, 103 ) )
   allocate ( c1, source = gen3  ( 203, 204, 205 ) )

   call foo ( b1 )
   call bar ( c1 )
   
   deallocate ( b1 )
   allocate ( b1, source = gen3 ( 104, 105, 106 ) )
   
   call foo ( b1 )

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child, gen3

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt

   select type ( dtv )
      type is ( base )
         write ( unit, * ) ' iotype:', iotype, ' v_list:', v_list(1)
         write ( fmt, * ) '(I', v_list(1),')'
         write ( unit, fmt, iostat = iostat )    dtv%i
      type is ( child )
         write ( unit, * ) ' iotype:', iotype, ' v_list:', v_list(1:2)
         write ( fmt, * ) '(I', v_list(1),', I', v_list(2),')'
         write ( unit, fmt, iostat = iostat )    dtv%i, dtv%j
      type is ( gen3 )
         write ( unit, * ) ' iotype:', iotype, ' v_list:', v_list(1:3)
         write ( fmt, * ) '(I', v_list(1),', I', v_list(2),', I',v_list(3),')'
         write ( unit, fmt, iostat = iostat )    dtv%i, dtv%j, dtv%k
   end select
   iomsg = 'dtiowrite'

end subroutine
