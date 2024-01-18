!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrCshiftLog.f
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* - data-pointers are module vars with protected attribute.
!* - the values other than association status of data-pointers are redefined
!*     outside module thr intrinsic assignment
!* - data-pointers' association status changed outside module by deallocate
!*      statement thr another pointers
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m

        logical*1, protected, pointer :: l1_p(:)
        logical*2, protected, pointer :: l2_p(:,:)
        logical*4, protected, pointer :: l4_p(:,:,:)
        logical*8, protected, pointer :: l8_p(:,:,:,:)

        contains
            subroutine allocation()

                allocate(l1_p(8), source=(/(logical(mod(i,2)==1,1), i=1,8)/))
                l1_p(2:9) => l1_p

                allocate(l2_p(4,4),source= reshape((/logical(l1_p(9:2:-1),2), &
			logical(l1_p,2) /), (/4,4/)))
                l2_p(0:,-0:) => l2_p


                allocate(l4_p(2,2,2), source = reshape(logical(l1_p,4), &
			(/2,2,2/)))
                l4_p(-0:,-1:,-0:) => l4_p

                allocate(l8_p(2,2,2,2), source=reshape( logical(l2_p,8), &
			(/2,2,2,2/)))

                l8_p(2:,2:,-127:,128:) => l8_p

            end subroutine

end module

program main
        use m

	logical*1, pointer :: l1_p1(:,:)
        logical*2, pointer :: l2_p1(:,:)
        logical*4, pointer :: l4_p1(:,:,:)
        logical*8, pointer :: l8_p1(:,:,:,:)

	call allocation

	if ( .not. associated(l4_p)) stop 11
	if ( any(lbound(l4_p) .ne. (/0,-1,0 /))) stop 13
	if ( any(ubound(l4_p) .ne. (/1,0,1 /))) stop 15

	if ( .not. associated(l2_p)) stop 7
	if ( any(lbound(l2_p) .ne. (/0,0 /))) stop 8
	if ( any(ubound(l2_p) .ne. (/3,3 /))) stop 9

	if ( .not. associated(l1_p)) stop 1
	if ( any(lbound(l1_p) .ne. (/2 /))) stop 3
	if ( any(ubound(l1_p) .ne. (/9 /))) stop 5

	if ( .not. associated(l8_p)) stop 21
	if ( any(lbound(l8_p) .ne. (/2,2,-127,128 /))) stop 23
	if ( any(ubound(l8_p) .ne. (/3,3,-126,129 /))) stop 25

	print *, l1_p
	print *, l2_p
	print *, l4_p
	print *, l8_p

	l1_p = l1_p(9:2:-1)
	l2_p = reshape((/ l2_p(0,:), l2_p(1,:), l2_p(2,:),l2_p(3,:) /), (/4,4/))
	l4_p = l4_p .and. .true.
	l8_p = l8_p .or. .false.

	print *, l1_p
	print *, l2_p
	print *, l4_p
	print *, l8_p

	print *, cshift(l1_p, -2)
	print *, cshift(l2_p, (/-1,0,1,-1/), 2)
	print *, cshift(l4_p, 1)
	print *, cshift(l8_p, -1)

	l1_p1(2:3,4:7) => l1_p
        deallocate(l1_p1)

	l2_p1(1:,3:) => l2_p
        deallocate(l2_p1)

	l4_p1(2:,4:,-9:) => l4_p
        deallocate(l4_p1)

	l8_p1(1:,1:,1:,1:) => l8_p
        deallocate(l8_p1)

    end program
