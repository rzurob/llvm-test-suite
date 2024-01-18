!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : cdasMultiple
!*
!*  PROGRAMMER                 : dforster
!*  DATE                       : 2010-11-18
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray dummy arguments - scalar
!*  SECONDARY FUNCTIONS TESTED : multiple images
!*  ADAPTED FROM               : cdaasMultiple <- cdaasBasicCopy
!*
!*  DESCRIPTION
!*
!*  Copy data from one machine to another, round-robin, until it arrives at the
!*  last machine.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program cdasMultiple
    implicit none

    integer, save :: ico[*] = 0
    real(8), save :: rco[*] = 0.0d0
    complex(8), save :: zco[*] = (0.0d0,0.0d0)
    character(4), save :: cco[*] = ''
    logical(1), save :: lco[*] = .false.
    integer :: i, curImage, nImages, nxtImage

    curImage = this_image()
    nImages  = num_images()
    nxtImage = curImage + 1
    if (nxtImage > nImages) nxtImage = 1

    ico = -(curImage-3)
    rco = 1.0d0 / ico
    zco = cmplx(real(curImage,kind(zco)), rco, kind(zco))
    cco = repeat(achar(iachar('A')+curImage), len(cco))
    lco = mod(curImage,2) == 0

    sync all

    do i = 1, nImages-1
      if (i == curImage) then
         call sub(ico, rco, zco, cco, lco, nxtImage) ! copy this array to the next one
      end if
      sync all
    end do

    if (curImage == nImages) then ! this should still look like #1
       print *, ico
       print *, rco
       print *, zco
       print *, cco
       print *, lco
    end if

  contains

    subroutine sub(iarr, rarr, zarr, carr, larr, im)
        integer :: im
        integer :: iarr[*]
        real(8) :: rarr[*]
        complex(8) :: zarr[*]
        character(4) :: carr[*]
        logical(1) :: larr[*]
        integer :: itmp
        real(8) :: rtmp
        complex(8) :: ztmp
        character(4) :: ctmp
        logical(1) :: ltmp

        ! Swap:
        itmp = iarr; iarr = iarr[im]; iarr[im] = itmp
        rtmp = rarr; rarr = rarr[im]; rarr[im] = rtmp
        ztmp = zarr; zarr = zarr[im]; zarr[im] = ztmp
        ctmp = carr; carr = carr[im]; carr[im] = ctmp
        ltmp = larr; larr = larr[im]; larr[im] = ltmp

    end subroutine sub

end program cdasMultiple
