! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/others/dataPtrDTIO.f
! opt variations: -qnol -qnodeferredlp

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
   type base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i
   end type

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(*,4)), intent(in) :: dtv
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
    type(base(:,4)), pointer :: p(:,:,:)
    type(base(:,4)), target, allocatable :: tar(:)

    allocate(tar(12), source=(/(base(20,4)(i), i=1,12)/))

    p(2:3, 4:6, -7:-6) => tar(12:1:-1)

    if ( .not. associated(p)) stop 1
    if ( any(lbound(p) .ne. (/2,4,-7/))) stop 2
    if ( any(ubound(p) .ne. (/3,6,-6/))) stop 3

    write(*, '(6DT)') p

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base

   class(base(*,4)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write (unit, *, iostat=iostat) dtv%i

   if (iostat /= 0) stop 5

end subroutine
