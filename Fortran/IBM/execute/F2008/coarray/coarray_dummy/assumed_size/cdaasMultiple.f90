!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : cdaasMultiple
!*
!*  DATE                       : 2010-09-30
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray dummy arguments - assumed size
!*  SECONDARY FUNCTIONS TESTED : multiple images
!*  ADAPTED FROM               : cdaasBasicCopy
!*
!*  DESCRIPTION
!*
!*  Copy data from one machine to another, round-robin, until it arrives at the
!*  last machine.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program cdaasMultiple
    implicit none

    integer, parameter :: SZ1 = 2, SZ2 = 3, SZ3 = 2
    integer, save :: ico(SZ1,SZ2,SZ3)[*] = 0
    real(8), save :: rco(SZ1,SZ2,SZ3)[*] = 0.0d0
    complex(8), save :: zco(SZ1,SZ2,SZ3)[*] = (0.0d0,0.0d0)
    character(4), save :: cco(SZ1,SZ2,SZ3)[*] = ''
    logical(1), save :: lco(SZ1,SZ2,SZ3)[*] = .false.
    integer :: i, j, k, curImage, nImages, nxtImage

    curImage = this_image()
    nImages  = num_images()
    nxtImage = curImage + 1
    if (nxtImage > nImages) nxtImage = 1

    ico = reshape([((curImage-1) * 20 + i, i=1,SZ1*SZ2*SZ3)], [SZ1,SZ2,SZ3])
    rco = 1.0d0 / ico
    zco = cmplx(real(curImage,kind(zco)), rco, kind(zco))
    cco = achar(iachar('A')+curImage-1) // reshape([(repeat(achar(iachar('a')+ i),3), i=0,SZ1*SZ2*SZ3-1)], [SZ1,SZ2,SZ3])
    lco = reshape([(mod(i,4) == curImage, i=1,SZ1*SZ2*SZ3)], [SZ1,SZ2,SZ3])

    sync all

    do i = 1, nImages-1
      if (i == curImage) then
         call sub(ico, rco, zco, cco, lco, SZ3, nxtImage) ! copy this array to the next one
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

    subroutine sub(iarr, rarr, zarr, carr, larr, n, im)
        integer :: n, im
        integer :: iarr(SZ1,SZ2,*)[*]
        real(8) :: rarr(SZ1,SZ2,*)[*]
        complex(8) :: zarr(SZ1,SZ2,*)[*]
        character(4) :: carr(SZ1,SZ2,*)[*]
        logical(1) :: larr(SZ1,SZ2,*)[*]
        integer :: itmp
        real(8) :: rtmp
        complex(8) :: ztmp
        character(4) :: ctmp
        logical(1) :: ltmp
        integer :: i, j, k

        do i = 1, SZ1
           do j = 1, SZ2
              do k = 1, n
                 itmp = iarr(i,j,k)
                 iarr(i,j,k) = iarr(i,j,k)[im]
                 iarr(i,j,k)[im] = itmp
                 rtmp = rarr(i,j,k)
                 rarr(i,j,k) = rarr(i,j,k)[im]
                 rarr(i,j,k)[im] = rtmp
                 ztmp = zarr(i,j,k)
                 zarr(i,j,k) = zarr(i,j,k)[im]
                 zarr(i,j,k)[im] = ztmp
                 ctmp = carr(i,j,k)
                 carr(i,j,k) = carr(i,j,k)[im]
                 carr(i,j,k)[im] = ctmp
                 ltmp = larr(i,j,k)
                 larr(i,j,k) = larr(i,j,k)[im]
                 larr(i,j,k)[im] = ltmp
              end do
           end do
        end do

    end subroutine sub

end program cdaasMultiple
