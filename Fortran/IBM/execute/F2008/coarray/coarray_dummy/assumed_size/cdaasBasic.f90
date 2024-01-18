!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : cdaasBasic
!*
!*  DATE                       : 2010-09-24
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray dummy arguments - assumed size
!*  SECONDARY FUNCTIONS TESTED : access only, same bounds in actual and dummy
!*  ADAPTED FROM               : -
!*
!*  DESCRIPTION
!*
!*  Pass various types of intrinsic array of different dimensions to subroutines
!*  expecting assumed-size array dummy arguments, and verify that the contents
!*  are correct.  (Here, we are only interested in verifying access to a coarray
!*  passed in as an assumed-size array.)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program cdaasBasic
    implicit none
    integer, parameter :: SZ1 = 2, SZ2 = 3, SZ3 = 2
    integer, parameter :: P = 1, Q = 2
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

    integer :: i, j, k, curImage, nImages
    logical(4) :: precision_r8, precision_x16
    external :: precision_r8, precision_x16

    curImage = this_image()
    nImages  = num_images()

    if (nImages < max(P,Q)) error stop 2

    ico = reshape([((curImage-1) * 20 + i, i=1,SZ1*SZ2*SZ3)], [SZ1,SZ2,SZ3])
    rco = 1.0d0 / ico
    zco = cmplx(real(curImage,kind(zco)), rco, kind(zco))
    cco = achar(iachar('A')+curImage-1) // reshape([(repeat(achar(iachar('a')+ i),3), i=0,SZ1*SZ2*SZ3-1)], [SZ1,SZ2,SZ3])
    lco = reshape([(mod(i,nImages) == curImage, i=1,SZ1*SZ2*SZ3)], [SZ1,SZ2,SZ3])

    qico = reshape([((Q-1) * 20 + i, i=1,SZ1*SZ2*SZ3)], [SZ1,SZ2,SZ3])
    qrco = 1.0d0 / qico
    qzco = cmplx(real(Q,kind(qzco)), qrco, kind(qzco))
    qcco = achar(iachar('A')+Q-1) // reshape([(repeat(achar(iachar('a')+ i),3), i=0,SZ1*SZ2*SZ3-1)], [SZ1,SZ2,SZ3])
    qlco = reshape([(mod(i,nImages) == Q, i=1,SZ1*SZ2*SZ3)], [SZ1,SZ2,SZ3])

    sync all

    call sub(ico, rco, zco, cco, lco, qico, qrco, qzco, qcco, qlco, SZ3, Q)

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
                 if (iarr(i,j,k)[im] /= qiarr(i,j,k)) error stop 3
                 if (carr(i,j,k)[im] /= qcarr(i,j,k)) error stop 4
                 if (larr(i,j,k)[im] .neqv. qlarr(i,j,k)) error stop 5
                 if (.not.precision_r8(rarr(i,j,k)[im], qrarr(i,j,k))) error stop 6
                 if (.not.precision_x16(zarr(i,j,k)[im], qzarr(i,j,k))) error stop 7
              end do
           end do
        end do

    end subroutine sub

end program cdaasBasic
