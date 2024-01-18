! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/intrinsic/dataPtrMergeInt2Log2.f
! opt variations: -qnok -qnol -qnodeferredlp

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrMergeInt2Log2.f
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* - data-pointer has declared type class(*), associated w logical*2 target
!* - the selector of data-pointer as arg of merge, ptr is redefined by itself
!*
!234567890123456789012345678901234567890123456789012345678901234567890

   module m
        type A(k1,n1)    ! (4,20)
            integer, kind :: k1
            integer, len  :: n1
            class(*), pointer :: Tsrc(:,:,:,:,:)
        end type
    end module

    module n
        use m

        integer*2, pointer :: tar(:)
    end module


    program main
        use n

        class(A(4,:)), allocatable :: a1
	logical*2, pointer :: lp(:,:,:,:,:)
	logical*2, target :: lt(32)= (/ (logical (mod(i,2) == 0, 2), i=1,32) /)

        allocate(A(4,20) :: a1)
        allocate(tar(32), source=(/( int(i,2),i=1,32  )/) )

	lp(1:2,0:1,0:1,-1:0,-2:-1) => lt(::1)

	if ( .not. associated(lp)) stop 2
	if ( any( lbound(lp) .ne. (/1,0,0,-1,-2/))) stop 5
	if ( any( ubound(lp) .ne. (/2,1,1,0,-1/))) stop 8

        a1%Tsrc(1:2,2:3,3:4,4:5,31:2**5) => tar

	if ( .not. associated(a1%Tsrc)) stop 12
	if ( any( lbound(a1%Tsrc) .ne. (/1,2,3,4,31 /))) stop 15
	if ( any( ubound(a1%Tsrc) .ne. (/2,3,4,5,32 /))) stop 18

        select type (x => a1%Tsrc)
            type is (integer*2)
                x = x(2:1:-1,3:2:-1,4:3:-1, 5:4:-1,2**5:31:-1)
		print *, x
		print *, merge(x, &
			reshape((/(88_2,i=1,32)/),(/2,2,2,2,2/)), lp)
            class default
                stop 12
        end select

    end program
