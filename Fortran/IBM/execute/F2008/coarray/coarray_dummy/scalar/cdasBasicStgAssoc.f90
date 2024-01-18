!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : cdasBasicStgAssoc
!*
!*  PROGRAMMER                 : dforster
!*  DATE                       : 2010-09-24
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray dummy arguments - assumed size
!*  SECONDARY FUNCTIONS TESTED : access only, storage association in multiple dimensions
!*  ADAPTED FROM               : cdaasBasic
!*
!*  DESCRIPTION
!*
!*  Like cdasBasic [see below], but pass in an element rather than an array, to
!*  test storage association.  In contrast to cdasStgAssoc, we use multiple
!*  dimensions in the subroutine.  Note: This is a little tougher, since the
!*  elements of the dummy arguments are not contiguous.  For that reason, we
!*  have to be extra careful what bounds we use.
!*  cdasBasic: "Pass various types of intrinsic scalars to a subroutine expecting
!*    same, and verify that the contents are correct.  (Here, we are only
!*    interested in verifying access to a coarray passed in as a scalar.)"
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program cdasBasicStgAssoc
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


    do i = 1, SZ1
       do j = 1, SZ2
          call sub1(ico(i,j,1), rco(i,j,1), zco(i,j,1), cco(i,j,1), lco(i,j,1), &
                    qico(i,j,1), qrco(i,j,1), qzco(i,j,1), qcco(i,j,1), qlco(i,j,1), SZ3, Q)
          call sub2(ico(i,j,1)[Q], rco(i,j,1)[Q], zco(i,j,1)[Q], cco(i,j,1)[Q], lco(i,j,1)[Q], &
                    qico(i,j,1), qrco(i,j,1), qzco(i,j,1), qcco(i,j,1), qlco(i,j,1), SZ3)
       end do
    end do

  contains

    subroutine sub1(iarr, rarr, zarr, carr, larr, qiarr, qrarr, qzarr, qcarr, qlarr, n, im)
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

        i = 1
        j = 1
        do k = 1, n
           if (iarr(i,j,k)[im] /= qiarr(i,j,k)) error stop 3
           if (carr(i,j,k)[im] /= qcarr(i,j,k)) error stop 4
           if (larr(i,j,k)[im] .neqv. qlarr(i,j,k)) error stop 5
           if (.not.precision_r8(rarr(i,j,k)[im], qrarr(i,j,k))) error stop 6
           if (.not.precision_x16(zarr(i,j,k)[im], qzarr(i,j,k))) error stop 7
        end do

    end subroutine sub1

    subroutine sub2(iarr, rarr, zarr, carr, larr, qiarr, qrarr, qzarr, qcarr, qlarr, n)
        integer :: n, im
        integer :: iarr(SZ1,SZ2,*)
        real(8) :: rarr(SZ1,SZ2,*)
        complex(8) :: zarr(SZ1,SZ2,*)
        character(4) :: carr(SZ1,SZ2,*)
        logical(1) :: larr(SZ1,SZ2,*)
        integer :: qiarr(SZ1,SZ2,*)
        real(8) :: qrarr(SZ1,SZ2,*)
        complex(8) :: qzarr(SZ1,SZ2,*)
        character(4) :: qcarr(SZ1,SZ2,*)
        logical(1) :: qlarr(SZ1,SZ2,*)
        integer :: i, j, k

        i = 1
        j = 1
        do k = 1, n
           if (iarr(i,j,k) /= qiarr(i,j,k)) error stop 13
           if (carr(i,j,k) /= qcarr(i,j,k)) error stop 14
           if (larr(i,j,k) .neqv. qlarr(i,j,k)) error stop 15
           if (.not.precision_r8(rarr(i,j,k), qrarr(i,j,k))) error stop 16
           if (.not.precision_x16(zarr(i,j,k), qzarr(i,j,k))) error stop 17
        end do

    end subroutine sub2

end program cdasBasicStgAssoc
