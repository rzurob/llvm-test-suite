! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/iostat/iostatenddtio001.f
! opt variations: -ql

!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan 9, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_end
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Ensure that basic funcationailty works for end and dtio
module m

   type base(d1)    ! (4)
      integer, kind :: d1
      integer(d1)   :: i = -9
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

   integer :: idx
   character(10) :: rbuffer(3)

end module

program scalar101
use m

   type(base(4)), allocatable :: b1

   integer :: stat=0
   character(150) :: msg

   open (1, file = 'iostatdtio.dat', form='formatted', access='sequential' )

   allocate ( b1, source = base(4)() )

   do while(.not. is_iostat_end(stat))
   read ( 1, *, iostat = stat, iomsg = msg )               b1
   if (is_iostat_end(stat)) then
    print *, "end of file"
   else
    print *, b1
   end if

   end do
end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, rbuffer, idx

   class(base(4)), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read ( unit, "(I4)", iostat = iostat ) dtv%i

   iomsg = 'dtioread'

end subroutine