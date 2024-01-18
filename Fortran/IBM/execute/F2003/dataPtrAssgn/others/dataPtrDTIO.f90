!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrDTIO.f
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* - data-ptr of a derived-type is used in DTIO write statement
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   type base
      integer :: i
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

end module

program main
    use m
    type(base), pointer :: p(:,:,:)
    type(base), target, allocatable :: tar(:)

    allocate(tar(12), source=(/(base(i), i=1,12)/))

    p(2:3, 4:6, -7:-6) => tar(12:1:-1)

    if ( .not. associated(p)) stop 1
    if ( any(lbound(p) .ne. (/2,4,-7/))) stop 2
    if ( any(ubound(p) .ne. (/3,6,-6/))) stop 3

    write(*, '(6DT)') p

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write (unit, *, iostat=iostat) dtv%i

   if (iostat /= 0) stop 5

end subroutine
