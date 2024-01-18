! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with namelist and object of public/private accessibility (Input)
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
   type :: base
      integer(4), private :: i
      contains
         procedure, pass :: seti
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

   class(base), private, allocatable :: b1
   class(base), private, pointer     :: b2
   type(base), private               :: b3
   integer :: stat
   character(150) :: msg

   namelist /n123/ b1, b2, b3
   private :: n123

   contains

   subroutine seti(dtv,i)
      class(base), intent(inout) :: dtv
      integer, intent(in) :: i
      dtv%i = i
   end subroutine

   subroutine start()
      allocate( b1, b2 )
      b1%i = 777
      b2%i = 888
      b3%i = 999
   end subroutine

   subroutine read123(unit)
      integer, intent(in) :: unit
      read ( unit, n123, iostat = stat, iomsg = msg )
   end subroutine

   subroutine check()
      if ( ( b1%i /= 2 )  .or. ( b2%i /= 1 ) .or. ( b3%i /= 3 ) ) error stop 1_4
   end subroutine
end module

program access101
use m

   open (1, file = 'access101.1', form='formatted', access='sequential' )

   call start()
   call read123(1)

   if ( (stat /= 0 ) .or. (msg /= 'dtioread' ) ) error stop 2_4

   call check()

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   integer(4) :: i
   if ( iotype /= "NAMELIST" ) error stop 3_4
   if ( size(v_list, 1) /= 0 ) error stop 4_4

   read (unit, *, iostat=iostat )   i

   call dtv%seti(i)

   iomsg = 'dtioread'

end subroutine
