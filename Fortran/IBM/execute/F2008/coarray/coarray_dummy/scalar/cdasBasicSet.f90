!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : cdasBasicSet
!*
!*  DATE                       : 2010-11-18
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray dummy arguments - scalar
!*  SECONDARY FUNCTIONS TESTED : set only
!*  ADAPTED FROM               : cdaasBasicSet <- cdaasBasic
!*
!*  DESCRIPTION
!*
!*  Like cdasBasic, but set the coarray instead of accessing it.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program cdasBasicSet
    implicit none

    integer, save :: ico[*] = 0
    real(8), save :: rco[*] = 0.0d0
    complex(8), save :: zco[*] = (0.0d0,0.0d0)
    character(4), save :: cco[*] = ''
    logical(1), save :: lco[*] = .false.

    integer :: qico
    real(8) :: qrco
    complex(8) :: qzco
    character(4) :: qcco
    logical(1) :: qlco

    integer :: curImage, nImages, nxtImage
    logical(4) :: precision_r8, precision_x16
    external :: precision_r8, precision_x16

    curImage = this_image()
    nImages  = num_images()
    nxtImage = curImage + 1
    if (nxtImage > nImages) nxtImage = 1

    qico = (nxtImage-1) * 20 + 1
    qrco = 1.0d0 / qico
    qzco = cmplx(real(nxtImage,kind(qzco)), qrco, kind(qzco))
    qcco = repeat(achar(iachar('A')+nxtImage-1), len(qcco))
    qlco = nxtImage == 1

    ! let every image set values for its neighbour "higher up"
    call sub(ico, rco, zco, cco, lco, qico, qrco, qzco, qcco, qlco, nxtImage)
    sync all

    ! now verify them
    qico = (curImage-1) * 20 + 1
    qrco = 1.0d0 / qico
    qzco = cmplx(real(curImage,kind(qzco)), qrco, kind(qzco))
    qcco = repeat(achar(iachar('A')+curImage-1), len(qcco))
    qlco = curImage == 1

    if (ico /= qico) error stop 3
    if (cco /= qcco) error stop 4
    if (lco .neqv. qlco) error stop 5
    if (.not.precision_r8(rco, qrco)) error stop 6
    if (.not.precision_x16(zco, qzco)) error stop 7

  contains

    subroutine sub(iarr, rarr, zarr, carr, larr, qiarr, qrarr, qzarr, qcarr, qlarr, im)
        integer :: im
        integer :: iarr[*]
        real(8) :: rarr[*]
        complex(8) :: zarr[*]
        character(4) :: carr[*]
        logical(1) :: larr[*]
        integer :: qiarr
        real(8) :: qrarr
        complex(8) :: qzarr
        character(4) :: qcarr
        logical(1) :: qlarr

        iarr[im] = qiarr
        carr[im] = qcarr
        larr[im] = qlarr
        rarr[im] = qrarr
        zarr[im] = qzarr

    end subroutine sub

end program cdasBasicSet
