!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : cdaasShapeCorankChg
!*
!*  PROGRAMMER                 : dforster
!*  DATE                       : 2010-09-29
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray dummy arguments - assumed size
!*  SECONDARY FUNCTIONS TESTED : both corank and dimensions are different
!*  ADAPTED FROM               : <fillIn>
!*
!*  DESCRIPTION
!*
!*  Like cdaasShapeChg, we copy elements from one image to another onto an
!*  assumed-size dummy argument coarray array, where the shape has been altered
!*  from the original, but the extent is respected.  Here we also change the
!*  corank in several ways.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program cdaasShapeCorankChg
    implicit none

    integer, parameter :: SZ1 = 2, SZ2 = 3, SZ3 = 4
    integer, parameter :: P = 1, Q = 2
    integer, save :: ico(SZ1,SZ2,SZ3)[2:3,*] = 0
    real(8), save :: rco(SZ1,SZ2,SZ3)[*] = 0.0d0
    complex(8), save :: zco(SZ1,SZ2,SZ3)[*] = (0.0d0,0.0d0)
    character(4), save :: cco(SZ1,SZ2,SZ3)[4:5,1,*] = ''
    logical(1), save :: lco(SZ1,SZ2,SZ3)[*] = .false.
    integer :: i, j, k, curImage, nImages

    curImage = this_image()
    nImages  = num_images()

    if (nImages < max(P,Q)) error stop 2

    ico = reshape([((curImage-1) * 20 + i, i=1,SZ1*SZ2*SZ3)], [SZ1,SZ2,SZ3])
    rco = 1.0d0 / ico
    zco = cmplx(real(curImage,kind(zco)), rco, kind(zco))
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
        integer :: iarr(SZ3,SZ2,*)[2,1,*]
        real(8) :: rarr(SZ3,SZ2,*)[1,2,*]
        complex(8) :: zarr(SZ3,SZ2,*)[1,1,*]
        character(4) :: carr(SZ3,SZ2,*)[*]
        logical(1) :: larr(SZ3,SZ2,*)[2,*]
        integer :: i, j, k

        do i = 1, SZ3
           do j = 1, SZ2
              do k = 1, n
                 ! These all access the same image, #2:
                 iarr(i,j,k)[im,1,1] = iarr(i,j,k)
                 rarr(i,j,k)[1,im,1] = rarr(i,j,k)
                 zarr(i,j,k)[1,1,im] = zarr(i,j,k)
                 carr(i,j,k)[im] = carr(i,j,k)
                 larr(i,j,k)[im,1] = larr(i,j,k)
              end do
           end do
        end do

    end subroutine sub

end program cdaasShapeCorankChg
