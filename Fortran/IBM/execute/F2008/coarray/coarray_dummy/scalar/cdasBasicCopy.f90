!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : cdasBasicCopy
!*
!*  PROGRAMMER                 : dforster
!*  DATE                       : 2010-11-18
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray dummy arguments - scalar
!*  SECONDARY FUNCTIONS TESTED : copy data from specific image (Q) to "this" (P)
!*  ADAPTED FROM               : cdaasBasicCopy <- cdaasBasic
!*
!*  DESCRIPTION
!*
!*  Pass various types of intrinsic scalars to a subroutine expecting same, and
!*  copy the contents to another image, checking that the update was correct.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program cdasBasicCopy
    implicit none
    integer, parameter :: P = 1, Q = 2
    integer, save :: ico[*] = 0
    real(8), save :: rco[*] = 0.0d0
    complex(8), save :: zco[*] = (0.0d0,0.0d0)
    character(4), save :: cco[*] = ''
    logical(1), save :: lco[*] = .false.
    integer :: curImage, nImages

    curImage = this_image()
    nImages  = num_images()

    if (nImages < max(P,Q)) error stop 2

    ico = -(curImage-3)
    rco = 1.0d0 / ico
    zco = cmplx(real(curImage,kind(zco)), rco, kind(zco))
    cco = repeat(achar(iachar('A')+curImage), len(cco))
    lco = mod(curImage,2) == 0

    sync all

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

  contains

    subroutine sub(iarr, rarr, zarr, carr, larr, im)
        integer :: im
        integer :: iarr[*]
        real(8) :: rarr[*]
        complex(8) :: zarr[*]
        character(4) :: carr[*]
        logical(1) :: larr[*]

        iarr[im] = iarr
        rarr[im] = rarr
        zarr[im] = zarr
        carr[im] = carr
        larr[im] = larr

    end subroutine sub

end program cdasBasicCopy
