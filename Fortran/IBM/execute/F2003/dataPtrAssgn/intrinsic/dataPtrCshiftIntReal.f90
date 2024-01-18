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
!* - date-pointers appear in common block statement, shared by diff scope unit
!*
!234567890123456789012345678901234567890123456789012345678901234567890

 program main

        integer*1, pointer :: int1_P(:)
        integer*2, pointer :: int2_P(:,:)
        integer*4, pointer :: int4_P(:,:,:,:)
        integer*8, pointer :: int8_P(:,:,:,:,:,:,:)

        real*4, pointer :: r4_P(:)
        real(8), pointer :: r8_P(:,:,:)
        real*16, pointer :: r6_P(:,:,:,:,:)

        common /blk/ int1_P, int2_P, int4_p, int8_p
        common /blk2/  r4_P, r8_P, r6_P

        allocate(int1_p(127), source=(/ ( int(i,1), i=1,127 ) /))

        allocate(int2_p(10,10), source = reshape( int(int1_p(1:100),2), &
		 (/10,10/) ))

        allocate(int4_p(3,3,3,3), source = reshape( int(int1_p(1:81),4), &
		 (/3,3,3,3/) ))

        allocate(int8_p(2,2,2,2,2,2,2), source = reshape( (/ 0_8, &
		 int(int1_p,8) /), (/2,2,2,2,2,2,2/) ))

	int8_p(size(int8_p):,max(3,4):,min(1,2):,2:,2:,2:,2:) => int8_p

        if ( .not. associated(int8_p)) stop 1
        if ( all(lbound(int8_p) .eq. (/128,4,1,2,2,2,2 /)) .neqv. .true.) stop 7
        if ( all(ubound(int8_p) .eq. (/129,5,2,3,3,3,3 /)) .neqv. .true.) stop 9

	int4_p(3:,2:,1:,0:) => int4_p

        if ( .not. associated(int4_p)) stop 11
        if ( all(lbound(int4_p) .eq. (/3,2,1,0 /)) .neqv. .true.) stop 17
        if ( all(ubound(int4_p) .eq. (/5,4,3,2 /)) .neqv. .true.) stop 19

	int2_p(size(array=int4_p,dim=1):,kind(0.0):) => int2_p

        if ( .not. associated(int2_p)) stop 21
        if ( all(lbound(int2_p) .eq. (/3,4 /)) .neqv. .true.) stop 27
        if ( all(ubound(int2_p) .eq. (/12, 13 /)) .neqv. .true.) stop 29

	int1_p(iachar('A'):iachar('a')) => int1_p(126:1:-2)

        if ( .not. associated(int1_p)) stop 31
        if ( lbound(int1_p,1) /= 65 ) stop 37
        if ( ubound(int1_p,1) /= 97 ) stop 39

	call int_sub

	call real_sub

	write(*, '(5f10.2)') r4_p
	write(*, '(4f12.4)') r8_p
	write(*, '(f12.4)') r6_p

    end program


   subroutine int_sub()
        integer*1, pointer :: int1_P(:)
        integer*2, pointer :: int2_P(:,:)
        integer*4, pointer :: int4_P(:,:,:,:)
        integer*8, pointer :: int8_P(:,:,:,:,:,:,:)
        common /blk/ int1_P, int2_P, int4_p, int8_p

	write (*, '(33I4)') int1_p
	write (*, '(10I4)') int2_p
	write (*, '(9I4)') int4_p
	write (*, '(16I4)') int8_p

	!print *, "calling cshift ..."

	write (*, '(33I4)') cshift(int1_p,shift=10)
	write (*, '(10I4)') cshift(int2_p, shift=-1, dim=1)
	write (*, '(9I4)') cshift(int4_p, shift=1, dim=3)
	write (*, '(16I4)') cshift(int8_p, shift=0, dim=7)

   end subroutine

   subroutine real_sub()
        real*4, pointer :: r4_P(:)
        real(8), pointer :: r8_P(:,:,:)
        real*16, pointer :: r6_P(:,:,:,:,:)

        common /blk2/  r4_P, r8_P, r6_P

	allocate(r4_p(10), source=(/ (real(i,4), i=1,10) /))
	allocate(r8_p(2,2,2), source=reshape( real(r4_p(3:10),8),(/2,2,2/)  ) )
	allocate(r6_p(1,1,2,1,1), source=reshape( real(r8_p(2,2,:),16), &
			(/1,1,2,1,1/)  ) )

	r4_p(1:5) => r4_p(::2)

        if ( .not. associated(r4_p)) stop 41
        if ( lbound(r4_p,1) /= 1 ) stop 47
        if ( ubound(r4_p,1) /= 5 ) stop 49

	r8_p(2:,2:,2:) => r8_p

        if ( .not. associated(r8_p)) stop 51
        if ( all(lbound(r8_p) .eq. (/2,2,2/)) .neqv. .true.) stop 57
        if ( all(ubound(r8_p) .eq. (/3,3,3 /)) .neqv. .true.) stop 59

	r6_p(1:,1:,1:,-1:,-1:) => r6_p(:,:,1::2,:,:)

        if ( .not. associated(r6_p)) stop 71
        if ( all(lbound(r6_p) .eq. (/1,1,1,-1,-1/)) .neqv. .true.) stop 77
        if ( all(ubound(r6_p) .eq. lbound(r6_p)) .neqv. .true.) stop 79

   end subroutine
