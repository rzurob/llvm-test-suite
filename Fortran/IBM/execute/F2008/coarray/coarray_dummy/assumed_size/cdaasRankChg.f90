!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-09-29
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray dummy arguments - assumed size
!*  SECONDARY FUNCTIONS TESTED : different rank (and dimensions) - actual vs. dummy
!*  ADAPTED FROM               : cdaasShapeChg
!*
!*  DESCRIPTION
!*
!*  Like cdaasShapeChg, but with different rank for dummy than for actual.
!*  This necessarily means different dimensions.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program cdaasRankChg
    implicit none
    integer, parameter :: SZ1 = 2, SZ2 = 3, SZ3 = 4
    integer, parameter :: P = 1, Q = 2
    integer, save :: ico(SZ1*SZ2*SZ3)[*] = 0
    real(8), save :: rco(SZ1,SZ2*SZ3)[*] = 0.0d0
    complex(8), save :: zco(SZ1*SZ2,SZ3)[*] = (0.0d0,0.0d0)
    character(4), save :: cco(SZ1,SZ2,SZ3)[*] = ''
    logical(1), save :: lco(SZ1,SZ2,SZ3)[*] = .false.
    integer :: i, j, k, curImage, nImages

    curImage = this_image()
    nImages  = num_images()

    if (nImages < max(P,Q)) error stop 2

    ico = [((curImage-1) * 20 + i, i=1,SZ1*SZ2*SZ3)]
    rco = reshape(1.0d0 / ico, [SZ1,SZ2*SZ3])
    zco = reshape(cmplx(real(curImage,kind(zco)), [rco], kind(zco)), [SZ1*SZ2,SZ3])
    cco = achar(iachar('A')+curImage-1) // reshape([(repeat(achar(iachar('a')+ i),3), i=0,SZ1*SZ2*SZ3-1)], [SZ1,SZ2,SZ3])
    lco = reshape([(mod(i,4) == curImage, i=1,SZ1*SZ2*SZ3)], [SZ1,SZ2,SZ3])

    sync all

    if (curImage == P) then
      call sub(ico, rco, zco, cco, lco, SZ1, Q)
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

    subroutine sub(iarr, rarr, zarr, carr, larr, n, im)
        integer :: n, im
        integer :: iarr(SZ3*SZ2,*)[*]
        real(8) :: rarr(SZ3,SZ2,*)[*]
        complex(8) :: zarr(SZ3,SZ2,*)[*]
        character(4) :: carr(SZ3*SZ2,*)[*]
        logical(1) :: larr(*)[*]
        integer :: i, j, k

        do i = 1, SZ3*SZ2
           do k = 1, n
              iarr(i,k)[im] = iarr(i,k)
              carr(i,k)[im] = carr(i,k)
           end do
        end do

        do i = 1, SZ3
           do j = 1, SZ2
              do k = 1, n
                 rarr(i,j,k)[im] = rarr(i,j,k)
                 zarr(i,j,k)[im] = zarr(i,j,k)
              end do
           end do
        end do

        do i = 1, SZ3*SZ2*n
           larr(i)[im] = larr(i)
        end do

    end subroutine sub

end program cdaasRankChg
