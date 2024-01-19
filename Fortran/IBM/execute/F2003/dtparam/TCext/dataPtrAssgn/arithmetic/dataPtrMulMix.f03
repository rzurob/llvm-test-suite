! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/dataPtrAssgn/arithmetic/dataPtrMulMix.f
! opt variations: -ql

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
!* - data_ptr is the component of a derived-type.
!* - apply the allocatable intrinsic assignment to array objects of the DT
!* - integer type * real type
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
     type base(k1)    ! (4)
         integer, kind         :: k1
         integer(k1) , pointer :: ptr(:)
     end type
end module

program main
    use m
    type(base(4)), allocatable ::  b1(:), b2(:)

    allocate(b1(5))
    allocate(b2(10))

    do i = 1, 5
        allocate(b1(i)%ptr(10), source = (/( (i-1)*10+j, j=1,10) /) )
        b1(i)%ptr(i:) => b1(i)%ptr

        if ( .not. associated(b1(i)%ptr)) call zzrc(i_4)
	if (lbound(b1(i)%ptr,1) /= i ) call zzrc(i_4+10)
	if (ubound(b1(i)%ptr,1) /= i+9 ) call zzrc(i_4+20)
    enddo

    do i = 1, 5
        allocate(b2(i)%ptr(5), source = (/( i*10+j, j=1,5) /) )
    enddo

    b1 = b2

    do i = 1, 5
        write(*, '(5f15.10)') b1(i)%ptr * real(2,8)
    enddo
end


