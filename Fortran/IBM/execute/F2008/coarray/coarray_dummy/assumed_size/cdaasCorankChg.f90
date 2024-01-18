!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : cdaasCorankChg
!*
!*  PROGRAMMER                 : dforster
!*  DATE                       : 2010-09-29
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray dummy arguments - assumed size
!*  SECONDARY FUNCTIONS TESTED : corank is different
!*  ADAPTED FROM               : cdaasRankChg
!*
!*  DESCRIPTION
!*
!*  Like cdaasRankChg, but we play with the corank instead.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program cdaasCorankChg

    implicit none
    integer, parameter :: SZ1 = 2, SZ2 = 3, SZ3 = 4
    integer, parameter :: P = 1, Q = 2, LCO1 = 1, UCO1 = 2, LCO2 = 1, UCO2 = 1, LCO3 = 1
    integer, save :: ico(SZ1,SZ2,SZ3)[LCO1:UCO1,LCO2:*] = 0
    real(8), save :: rco(SZ1,SZ2,SZ3)[LCO2:*] = 0.0d0
    complex(8), save :: zco(SZ1,SZ2,SZ3)[LCO1:UCO1,LCO2:UCO2,LCO3:*] = (0.0d0,0.0d0)
    character(4), save :: cco(SZ1,SZ2,SZ3)[*] = ''
    logical(1), save :: lco(SZ1,SZ2,SZ3)[LCO1:UCO1,LCO2:*] = .false.
    integer :: i, j, k, curImage, nImages

    curImage = this_image()
    nImages  = num_images()

    if (nImages < max(P,Q)) error stop 2

    ico = reshape([((curImage-1) * 20 + i, i=1,SZ1*SZ2*SZ3)], [SZ1,SZ2,SZ3])
    rco = 1.0d0 / ico
    zco = reshape(cmplx(real(curImage,kind(zco)), [rco], kind(zco)), [SZ1,SZ2,SZ3])
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
        integer :: iarr(SZ3,SZ2,*)[LCO1:UCO1,LCO2:*] ! same corank
        real(8) :: rarr(SZ3,SZ2,*)[LCO1:UCO1,LCO2:*] ! more
        complex(8) :: zarr(SZ3,SZ2,*)[LCO1:UCO1,LCO2:*] ! less
        character(4) :: carr(SZ3,SZ2,*)[LCO1:UCO1,LCO2:UCO2,LCO3:*] ! more
        logical(1) :: larr(SZ3,SZ2,*)[*] ! less
        integer :: i, j, k

        do i = 1, SZ3
           do j = 1, SZ2
              do k = 1, n
                 iarr(i,j,k)[im,1] = iarr(i,j,k)
                 carr(i,j,k)[im,1,1] = carr(i,j,k)
                 rarr(i,j,k)[im,1] = rarr(i,j,k)
                 zarr(i,j,k)[im,1] = zarr(i,j,k)
                 larr(i,j,k)[im] = larr(i,j,k)
              end do
           end do
        end do

    end subroutine sub

end program cdaasCorankChg
