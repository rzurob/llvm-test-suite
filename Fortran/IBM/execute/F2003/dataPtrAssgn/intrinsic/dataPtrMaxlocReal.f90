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
!* - data-pointers are component of DT, type real*8,16 and class(*)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

        type base
            class(*), pointer :: up(:,:)
        end type

        type, extends(base) :: container
            real*8, pointer :: rp8(:,:,:)
            real*16, pointer :: rp16(:,:,:,:)
        end type
    end module

    program main

        use m

        type(container) :: c

        allocate(c%rp16(2,1,2,1))
        allocate(c%rp8(2,2,2))
        allocate(real*4::c%up(4,4))

        do i = 1, 4
        do j = 1, 4
            select type(x => c%up)
                type is(real*4)
                    x(j,i) = j*10 + i
                class default
                    stop 11
            end select
        enddo
        enddo

        select type(x => c%up)
            type is (real*4)
                c%rp8 = reshape(real(x(:,::2),8), (/2,2,2/) )
        	c%rp16 = reshape( real(x(4:1:-2,4:1:-2),16), (/2,1,2,1/))
            class default
                stop 13
        end select

        c%up(1:, 1:) => c%up(::2,:)
        if ( .not. associated(c%up)) stop 15
        if ( any ( lbound(c%up) .ne. (/1,1/) )) stop 17
        if ( any ( ubound(c%up) .ne. (/2,4/) )) stop 19

        select type(x => c%up)
            type is (real*4)
		write (*, '(8f8.3)') x
		print *,  maxloc(x, dim=1, kind=1)
            class default
                stop 23
        end select

        c%rp8(lbound(c%up,1,8):,size(c%up,2):,2:) => c%rp8(::2,::2,::2)
        if ( .not. associated(c%up)) stop 25
        if ( any ( lbound(c%rp8) .ne. (/1,4,2/) )) stop 27
        if ( any ( lbound(c%rp8) .ne. ubound(c%rp8) )) stop 29

	write (*, '(f10.5)') c%rp8
	print *,  maxloc(c%rp8, dim=2, kind=2)

        c%rp16(i:, j:, i+j:, i-j:) => c%rp16
        if ( .not. associated(c%rp16)) stop 35
        if ( any ( lbound(c%rp16) .ne. (/5,5,10,0/) )) stop 37
        if ( any ( ubound(c%rp16) .ne. (/6,5,11,0/) )) stop 39

	write (*, '(4f20.15)') 	c%rp16
	print *,  maxloc(c%rp16, kind=8)

    end program
