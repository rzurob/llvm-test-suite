!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : cdasCodimensionChg
!*
!*  DATE                       : 2010-11-18
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray dummy arguments - scalar
!*  SECONDARY FUNCTIONS TESTED : different codimensions, but same corank
!*  ADAPTED FROM               : cdaasCorankChg <- cdaasRankChg
!*  ADAPTED FROM               : cdasCorankChg ()
!*
!*  DESCRIPTION
!*
!*  Copy elements from one image to another using a dummy argument coarray scalar
!*  with different codimensions (but the same corank).
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program cdasCodimensionChg

    implicit none
    integer, parameter :: P = 1, Q = 2, LCO1 = 1, UCO1 = 2, LCO2 = 2, UCO2 = 2, LCO3 = 3
    integer, save :: ico[LCO1:UCO1,LCO2:*] = 0
    real(8), save :: rco[LCO2:*] = 0.0d0
    complex(8), save :: zco[LCO1:UCO1,LCO2:UCO2,LCO3:*] = (0.0d0,0.0d0)
    character(4), save :: cco[*] = ''
    logical(1), save :: lco[LCO1:UCO1,LCO2:*] = .false.
    integer :: curImage, nImages

    curImage = this_image()
    nImages  = num_images()

    if (nImages < max(P,Q)) error stop 2

    ico = -(curImage - 3)
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
        integer :: iarr[LCO2:UCO2,LCO1:*]                ! 2:2,1:*     from [LCO1:UCO1,LCO2:*]           1:2,2:*
        real(8) :: rarr[LCO1:*]                          ! 1:*         from [LCO2:*]                     2:*
        complex(8) :: zarr[LCO2:UCO2,LCO1:UCO1,LCO3:*]   ! 2:2,1:2,3:* from [LCO1:UCO1,LCO2:UCO2,LCO3:*] 1:2,2:2,3:*
        character(4) :: carr[LCO2:*]                     ! 2:*         from [*]                          1:*
        logical(1) :: larr[LCO1:UCO1,LCO2:*]             ! 1:2,2:*     from [LCO1:UCO1,LCO2:*]           1:2,2:*

        iarr[LCO2,im] = iarr
        rarr[im] = rarr
        zarr[LCO2,im,LCO3] = zarr
        carr[im+LCO2-1] = carr
        larr[im,LCO2] = larr

    end subroutine sub

end program cdasCodimensionChg
