! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/dataPtrAssgn/arithmetic/dataPtrAddReal1.f
! opt variations: -qnok -ql

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* - data-ptr is arg of elemental defined operator function
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

  integer :: key = 0

  type base(k1)    ! (4)
      integer, kind :: k1
      class(*), pointer :: ptr
  end type

  interface operator (+)
	elemental function add(in1, in2)
	    import base
	    real*16, intent(in) :: in1
	    type(base(4)), intent(in) :: in2
	end function
  end interface

end module

program main
    use m

    real*16, pointer :: ptr(:)
    class(base(4)), allocatable :: b1(:)
    logical precision_r6

    allocate(ptr(20), source = (/(real(i,16), i=1,20 )/) )

    ptr(2:) => ptr(::2)

    if ( .not. associated(ptr)) error stop 11
    if ( lbound(ptr,1) /= 2 ) error stop 12
    if ( ubound(ptr,1) /= 11 ) error stop 13

    allocate(b1(10))

    do i = 1, 10
         b1(i)%ptr => ptr(11-i+1)
         if ( .not. associated(b1(i)%ptr, ptr(11-i+1))) error stop 11
    end do

    ptr = ptr + b1

    if ( key /= 0 ) error stop 12
    if ( .not. precision_r6(ptr, real(20,16))) error stop 13

end program

       elemental function add(in1, in2)
	   use m, only:base
	   real*16, intent(in) :: in1
           type(base(4)), intent(in) :: in2
	   real*16 add

	   select type(x => in2%ptr)
		type is (real*16)
          	    add = in1 + x
	  	class default
		    key = 1
	   end select
       end function

