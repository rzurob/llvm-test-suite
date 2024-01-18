!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : cdaasBasicSet
!*
!*  PROGRAMMER                 : dforster
!*  DATE                       : 2010-09-29
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray dummy arguments - assumed size
!*  SECONDARY FUNCTIONS TESTED : set only, same bounds in actual and dummy
!*  ADAPTED FROM               : cdaasBasic
!*
!*  DESCRIPTION
!*
!*  Like cdaasBasic, but set the coarray instead of accessing it.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program cdaasBasicSet
    implicit none
    integer, parameter :: SZ1 = 2, SZ2 = 3, SZ3 = 2

    integer, save :: ico(SZ1,SZ2,SZ3)[*] = 0
    real(8), save :: rco(SZ1,SZ2,SZ3)[*] = 0.0d0
    complex(8), save :: zco(SZ1,SZ2,SZ3)[*] = (0.0d0,0.0d0)
    character(4), save :: cco(SZ1,SZ2,SZ3)[*] = ''
    logical(1), save :: lco(SZ1,SZ2,SZ3)[*] = .false.

    integer :: qico(SZ1,SZ2,SZ3)
    real(8) :: qrco(SZ1,SZ2,SZ3)
    complex(8) :: qzco(SZ1,SZ2,SZ3)
    character(4) :: qcco(SZ1,SZ2,SZ3)
    logical(1) :: qlco(SZ1,SZ2,SZ3)

    integer :: i, j, k, curImage, nImages, nxtImage
    logical(4) :: precision_r8, precision_x16
    external :: precision_r8, precision_x16

    curImage = this_image()
    nImages  = num_images()
    nxtImage = curImage + 1
    if (nxtImage > nImages) nxtImage = 1

    qico = reshape([((nxtImage-1) * 20 + i, i=1,SZ1*SZ2*SZ3)], [SZ1,SZ2,SZ3])
    qrco = 1.0d0 / qico
    qzco = cmplx(real(nxtImage,kind(qzco)), qrco, kind(qzco))
    qcco = achar(iachar('A')+nxtImage-1) // reshape([(repeat(achar(iachar('a')+ i),3), i=0,SZ1*SZ2*SZ3-1)], [SZ1,SZ2,SZ3])
    qlco = reshape([(mod(i,nImages) == nxtImage, i=1,SZ1*SZ2*SZ3)], [SZ1,SZ2,SZ3])

    ! let every image set values for its neighbour "higher up"
    call sub(ico, rco, zco, cco, lco, qico, qrco, qzco, qcco, qlco, SZ3, nxtImage)
    sync all

    ! now verify them
    qico = reshape([((curImage-1) * 20 + i, i=1,SZ1*SZ2*SZ3)], [SZ1,SZ2,SZ3])
    qrco = 1.0d0 / qico
    qzco = cmplx(real(curImage,kind(qzco)), qrco, kind(qzco))
    qcco = achar(iachar('A')+curImage-1) // reshape([(repeat(achar(iachar('a')+ i),3), i=0,SZ1*SZ2*SZ3-1)], [SZ1,SZ2,SZ3])
    qlco = reshape([(mod(i,nImages) == curImage, i=1,SZ1*SZ2*SZ3)], [SZ1,SZ2,SZ3])

    do i = 1, SZ1
       do j = 1, SZ2
          do k = 1, SZ3
             if (ico(i,j,k) /= qico(i,j,k)) error stop 3
             if (cco(i,j,k) /= qcco(i,j,k)) error stop 4
             if (lco(i,j,k) .neqv. qlco(i,j,k)) error stop 5
             if (.not.precision_r8(rco(i,j,k), qrco(i,j,k))) error stop 6
             if (.not.precision_x16(zco(i,j,k), qzco(i,j,k))) error stop 7
          end do
       end do
    end do

  contains

    subroutine sub(iarr, rarr, zarr, carr, larr, qiarr, qrarr, qzarr, qcarr, qlarr, n, im)
        integer :: n, im
        integer :: iarr(SZ1,SZ2,*)[*]
        real(8) :: rarr(SZ1,SZ2,*)[*]
        complex(8) :: zarr(SZ1,SZ2,*)[*]
        character(4) :: carr(SZ1,SZ2,*)[*]
        logical(1) :: larr(SZ1,SZ2,*)[*]
        integer :: qiarr(SZ1,SZ2,*)
        real(8) :: qrarr(SZ1,SZ2,*)
        complex(8) :: qzarr(SZ1,SZ2,*)
        character(4) :: qcarr(SZ1,SZ2,*)
        logical(1) :: qlarr(SZ1,SZ2,*)
        integer :: i, j, k

        do i = 1, SZ1
           do j = 1, SZ2
              do k = 1, n
                 iarr(i,j,k)[im] = qiarr(i,j,k)
                 carr(i,j,k)[im] = qcarr(i,j,k)
                 larr(i,j,k)[im] = qlarr(i,j,k)
                 rarr(i,j,k)[im] = qrarr(i,j,k)
                 zarr(i,j,k)[im] = qzarr(i,j,k)
              end do
           end do
        end do

    end subroutine sub

end program cdaasBasicSet
