!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : cdaasBasicNewBounds
!*
!*  PROGRAMMER                 : dforster
!*  DATE                       : 2010-09-28
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray dummy arguments - assumed size
!*  SECONDARY FUNCTIONS TESTED : everything but the bounds are the same
!*  ADAPTED FROM               : cdaasBasic
!*
!*  DESCRIPTION
!*
!*  Like the above, but use different bounds (extent is the same, though).
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program cdaasBasicNewBounds
    implicit none
    integer, parameter :: SZ1 = 2, SZ2 = 3, SZ3 = 4
    integer, parameter :: P = 1, Q = 2
    integer, save :: ico(SZ1,SZ2,SZ3)[*] = 0
    real(8), save :: rco(SZ1,SZ2,SZ3)[*] = 0.0d0
    complex(8), save :: zco(SZ1,SZ2,SZ3)[*] = (0.0d0,0.0d0)
    character(4), save :: cco(SZ1,SZ2,SZ3)[*] = ''
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
      call sub(ico, rco, zco, cco, lco, SZ3, Q)
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
        integer :: iarr(2:SZ2+1,3:SZ1+2,2:*)[*]
        real(8) :: rarr(3:SZ2+2,4:SZ1+3,3:*)[*]
        complex(8) :: zarr(-1:SZ2-2,2:SZ1+1,-1:*)[*]
        character(4) :: carr(0:SZ2-1,0:SZ1-1,0:*)[*]
        logical(1) :: larr(111:SZ2+110,1000000:SZ1+999999,*)[*]
        integer :: i, j, k

        do i = 1, SZ2
           do j = 1, SZ1
              do k = 1, n
                 iarr(i+1,j+2,k+1)[im] = iarr(i+1,j+2,k+1)
                 rarr(i+2,j+3,k+2)[im] = rarr(i+2,j+3,k+2)
                 zarr(i-2,j+1,k-2)[im] = zarr(i-2,j+1,k-2)
                 carr(i-1,j-1,k-1)[im] = carr(i-1,j-1,k-1)
                 larr(i+110,j+999999,k)[im] = larr(i+110,j+999999,k)
              end do
           end do
        end do

    end subroutine sub

end program cdaasBasicNewBounds
