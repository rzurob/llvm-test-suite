!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : cdasCorankChg
!*
!*  PROGRAMMER                 : dforster
!*  DATE                       : 2010-11-18
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray dummy arguments - scalar
!*  SECONDARY FUNCTIONS TESTED : corank is different
!*  ADAPTED FROM               : cdaasCorankChg <- cdaasRankChg
!*
!*  DESCRIPTION
!*
!*  Copy elements from one image to another using a dummy argument coarray scalar
!*  with different corank (and necessarily different codimensions).
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program cdasCorankChg

	implicit none
	integer, parameter :: P = 1, Q = 2, LCO1 = 1, UCO1 = 2, LCO2 = 1, UCO2 = 2, LCO3 = 1
	integer, save :: ico[LCO1:UCO1,LCO2:*] = 0
	real(8), save :: rco[LCO2:*] = 0.0d0
	complex(8), save :: zco[LCO1:UCO1,LCO2:UCO2,LCO3:*] = (0.0d0,0.0d0)
	character(4), save :: cco[*] = ''
	logical(1), save :: lco[LCO1:UCO1,LCO2:*] = .false.
	integer :: i, j, k, curImage, nImages

	curImage = this_image()
	nImages  = num_images()

	ico = -(curImage - 3)
	rco = 1.0d0 / ico
	zco = cmplx(real(curImage,kind(zco)), rco, kind(zco))
	cco = repeat(achar(iachar('A')+curImage), len(cco))
	lco = mod(curImage,2) == 0
	sync all

	if (nImages > 1) then
		if (curImage == P) then
			call sub(ico, rco, zco, cco, lco, Q)
		end if

		sync all

		if (curImage == Q) then
			print *, ico
			print *, rco
			print *, zco
			print *, cco
			print *, lco
		end if
	else
		print *, ico
		print *, rco
		print *, zco
		print *, cco
		print *, lco
	end if

contains

	subroutine sub(iarr, rarr, zarr, carr, larr, im)
		integer :: im
		integer :: iarr[LCO1:UCO1,LCO2:*]                ! same corank
		real(8) :: rarr[LCO1:UCO1,LCO2:*]                ! more
		complex(8) :: zarr[LCO1:UCO1,LCO2:*]             ! less
		character(4) :: carr[LCO1:UCO1,LCO2:UCO2,LCO3:*] ! more
		logical(1) :: larr[*]                            ! less

		iarr[im,1] = iarr
		carr[im,1,1] = carr
		rarr[im,1] = rarr
		zarr[im,1] = zarr
		larr[im] = larr
	end subroutine sub

end program cdasCorankChg
