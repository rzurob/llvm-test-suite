! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/dataPtrAssgn/others/dataPtrDTIO1.f
! opt variations: -ql

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrDTIO1.f
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* - data-ptr is a component of a derived-type is used in DTIO write statement,
!*   whose routine is a type-bound procedure
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   type base(k1)    ! (4)
      integer, kind         :: k1
      class(*), pointer :: p(:)
      real(k1), allocatable :: tar(:)

      contains
	  procedure :: writeformatted
	  generic :: write(formatted) => writeformatted
   end type

   contains

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)

   class(base(4)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type(x=>dtv%p)
	type is (real)
            write (unit, '(5g12.6)', iostat=iostat) x
        class default
            stop 4
   end select

   if (iostat /= 0) stop 5

end subroutine

end module

program main
    use m

    type(base(4)), pointer :: b1

    allocate(b1)

    allocate(b1%tar(10), source = (/(real(i,4),i=-10,-1)/))

    b1%p(size(b1%tar):) => b1%tar(::2)

    if ( .not. associated(b1%p, b1%tar(::2))) stop 1
    if ( lbound(b1%p,1) /= 10 ) stop 2
    if ( ubound(b1%p,1) /= 14 ) stop 3

    write(*, '(DT)') b1

end program

