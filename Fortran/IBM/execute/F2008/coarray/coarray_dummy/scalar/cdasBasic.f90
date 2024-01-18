!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : cdasBasic
!*
!*  DATE                       : 2010-11-18
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray dummy arguments - scalar
!*  SECONDARY FUNCTIONS TESTED : access only
!*  ADAPTED FROM               : cdaasBasic
!*
!*  DESCRIPTION
!*
!*  Pass various types of intrinsic scalars to a subroutine expecting same, and
!*  verify that the contents are correct.  (Here, we are only interested in
!*  verifying access to a coarray passed in as a scalar.)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program cdasBasic
    implicit none
    integer, parameter :: P = 1, Q = 2
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

    integer :: curImage, nImages
    logical(4) :: precision_r8, precision_x16
    external :: precision_r8, precision_x16

    curImage = this_image()
    nImages  = num_images()

    if (nImages < max(P,Q)) error stop 2

    ico = (curImage-1) * 20 + 1
    rco = 1.0d0 / ico
    zco = cmplx(real(curImage,kind(zco)), rco, kind(zco))
    cco = repeat(achar(iachar('A')+curImage-1), len(cco))
    lco = curImage == 1

    qico = (Q-1) * 20 + 1
    qrco = 1.0d0 / qico
    qzco = cmplx(real(Q,kind(qzco)), qrco, kind(qzco))
    qcco = repeat(achar(iachar('A')+Q-1), len(qcco))
    qlco = Q == 1

    sync all

    call sub(ico, rco, zco, cco, lco, qico, qrco, qzco, qcco, qlco, Q)

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

        if (iarr[im] /= qiarr) error stop 3
        if (carr[im] /= qcarr) error stop 4
        if (larr[im] .neqv. qlarr) error stop 5
        if (.not.precision_r8(rarr[im], qrarr)) error stop 6
        if (.not.precision_x16(zarr[im], qzarr)) error stop 7

    end subroutine sub

end program cdasBasic
