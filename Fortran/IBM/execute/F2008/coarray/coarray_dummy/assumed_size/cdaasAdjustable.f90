!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : cdaasAdjustable
!*
!*  PROGRAMMER                 : dforster
!*  DATE                       : 2010-09-29
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray dummy arguments - assumed size
!*  SECONDARY FUNCTIONS TESTED : dimensions and codimensions subject to other dummy arguments (adjustable)
!*  ADAPTED FROM               : cdaasModule
!*
!*  DESCRIPTION
!*
!*  Like cdaasModule, but adds adjustability to the mix (we pass in arguments
!*  to control lower and upper bounds, codimensions, etc.).
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module cdaasAdjustableMod
    integer, parameter :: SZ1 = 2, SZ2 = 3, SZ3 = 2
    integer, parameter :: P = 1, Q = 2
    integer, save :: ico(SZ1,SZ2,SZ3)[*] = 0
    real(8), save :: rco(SZ1,SZ2,SZ3)[*] = 0.0d0
    complex(8), save :: zco(SZ1,SZ2,SZ3)[*] = (0.0d0,0.0d0)
    character(4), save :: cco(SZ1,SZ2,SZ3)[*] = ''
    logical(1), save :: lco(SZ1,SZ2,SZ3)[*] = .false.

  contains

    subroutine submod(iarr, rarr, zarr, carr, larr, l1, u1, l2, u2, lco1, uco1, lco2, im1, im2)
        integer :: l1, u1, l2, u2, lco1, uco1, lco2, im1, im2
        integer :: iarr(l1:u1,l2:*)[lco1:uco1,lco2:*], itmp
        real(8) :: rarr(l1:u1,l2:*)[lco1:uco1,lco2:*], rtmp
        complex(8) :: zarr(l1:u1,l2:*)[lco1:uco1,lco2:*], ztmp
        character(4) :: carr(l1:u1,l2:*)[lco1:uco1,lco2:*], ctmp
        logical(1) :: larr(l1:u1,l2:*)[lco1:uco1,lco2:*], ltmp
        integer :: i, j, k

        do i = l1, u1
           do k = l2, u2
              itmp = iarr(i,k)
              iarr(i,k) = iarr(i,k)[im1,im2]
              iarr(i,k)[im1,im2] = itmp

              rtmp = rarr(i,k)
              rarr(i,k) = rarr(i,k)[im1,im2]
              rarr(i,k)[im1,im2] = rtmp

              ztmp = zarr(i,k)
              zarr(i,k) = zarr(i,k)[im1,im2]
              zarr(i,k)[im1,im2] = ztmp

              ctmp = carr(i,k)
              carr(i,k) = carr(i,k)[im1,im2]
              carr(i,k)[im1,im2] = ctmp

              ltmp = larr(i,k)
              larr(i,k) = larr(i,k)[im1,im2]
              larr(i,k)[im1,im2] = ltmp
           end do
        end do

    end subroutine submod

end module cdaasAdjustableMod


program cdaasAdjustable

    use :: cdaasAdjustableMod
    implicit none

    interface
        subroutine subext(iarr, rarr, zarr, carr, larr, l1, u1, l2, u2, lco1, uco1, lco2, im1, im2)
            integer :: l1, u1, l2, u2, lco1, uco1, lco2, im1, im2
	        integer :: iarr(l1:u1,l2:*)[lco1:uco1,lco2:*]
	        real(8) :: rarr(l1:u1,l2:*)[lco1:uco1,lco2:*]
	        complex(8) :: zarr(l1:u1,l2:*)[lco1:uco1,lco2:*]
	        character(4) :: carr(l1:u1,l2:*)[lco1:uco1,lco2:*]
	        logical(1) :: larr(l1:u1,l2:*)[lco1:uco1,lco2:*]
        end subroutine subext
    end interface

    integer :: i, j, k, curImage, nImages

    curImage = this_image()
    nImages  = num_images()

    if (nImages < max(P,Q)) error stop 2

    ico = reshape([((curImage-1) * 20 + i, i=1,SZ1*SZ2*SZ3)], [SZ1,SZ2,SZ3])
    rco = 1.0d0 / ico
    zco = cmplx(real(curImage,kind(zco)), rco, kind(zco))
    cco = achar(iachar('A')+curImage-1) // reshape([(repeat(achar(iachar('a')+ i),3), i=0,SZ1*SZ2*SZ3-1)], [SZ1,SZ2,SZ3])
    lco = reshape([(mod(i,4) == curImage, i=1,SZ1*SZ2*SZ3)], [SZ1,SZ2,SZ3])

    ! this is the boring way to test it, but it makes the verification easier:
    ! sync, make a change, sync, output it, sync, make another change, sync, output it, etc.
    sync all
    if (curImage == P) then
      call sub(ico, rco, zco, cco, lco, 2, 9, -1, 1, 1, 2, 1, 2, 1)
    end if

    sync all
    if (curImage == Q) then ! expect P's values
      print *, ico
      print *, rco
      print *, zco
      print *, cco
      print *, lco
    end if

    sync all
    if (curImage == P) then
      call submod(ico, rco, zco, cco, lco, -2, 3, 1, 4, 3, 3, 4, 3, 5)
    end if

    sync all
    if (curImage == Q) then ! expect Q's original values
      print *, ico
      print *, rco
      print *, zco
      print *, cco
      print *, lco
    end if

    sync all
    if (curImage == P) then
      call subext(ico, rco, zco, cco, lco, -1, 1, 2, 9, 1, 2, 1, 2, 1)
    end if

    sync all
    if (curImage == Q) then ! expect P's values again
      print *, ico
      print *, rco
      print *, zco
      print *, cco
      print *, lco
    end if

  contains

    subroutine sub(iarr, rarr, zarr, carr, larr, l1, u1, l2, u2, lco1, uco1, lco2, im1, im2)
        integer :: l1, u1, l2, u2, lco1, uco1, lco2, im1, im2
        integer :: iarr(l1:u1,l2:*)[lco1:uco1,lco2:*], itmp
        real(8) :: rarr(l1:u1,l2:*)[lco1:uco1,lco2:*], rtmp
        complex(8) :: zarr(l1:u1,l2:*)[lco1:uco1,lco2:*], ztmp
        character(4) :: carr(l1:u1,l2:*)[lco1:uco1,lco2:*], ctmp
        logical(1) :: larr(l1:u1,l2:*)[lco1:uco1,lco2:*], ltmp
        integer :: i, j, k

        do i = l1, u1
           do j = l2, u2
             itmp = iarr(i,j)
             iarr(i,j) = iarr(i,j)[im1,im2]
             iarr(i,j)[im1,im2] = itmp
             rtmp = rarr(i,j)
             rarr(i,j) = rarr(i,j)[im1,im2]
             rarr(i,j)[im1,im2] = rtmp
             ztmp = zarr(i,j)
             zarr(i,j) = zarr(i,j)[im1,im2]
             zarr(i,j)[im1,im2] = ztmp
             ctmp = carr(i,j)
             carr(i,j) = carr(i,j)[im1,im2]
             carr(i,j)[im1,im2] = ctmp
             ltmp = larr(i,j)
             larr(i,j) = larr(i,j)[im1,im2]
             larr(i,j)[im1,im2] = ltmp
           end do
        end do

    end subroutine sub

end program cdaasAdjustable


subroutine subext(iarr, rarr, zarr, carr, larr, l1, u1, l2, u2, lco1, uco1, lco2, im1, im2)
    integer :: l1, u1, l2, u2, lco1, uco1, lco2, im1, im2
    integer :: iarr(l1:u1,l2:*)[lco1:uco1,lco2:*], itmp
    real(8) :: rarr(l1:u1,l2:*)[lco1:uco1,lco2:*], rtmp
    complex(8) :: zarr(l1:u1,l2:*)[lco1:uco1,lco2:*], ztmp
    character(4) :: carr(l1:u1,l2:*)[lco1:uco1,lco2:*], ctmp
    logical(1) :: larr(l1:u1,l2:*)[lco1:uco1,lco2:*], ltmp
    integer :: i, j, k

    do i = l1, u1
       do j = l2, u2
          itmp = iarr(i,j)
          iarr(i,j) = iarr(i,j)[im1,im2]
          iarr(i,j)[im1,im2] = itmp
          rtmp = rarr(i,j)
          rarr(i,j) = rarr(i,j)[im1,im2]
          rarr(i,j)[im1,im2] = rtmp
          ztmp = zarr(i,j)
          zarr(i,j) = zarr(i,j)[im1,im2]
          zarr(i,j)[im1,im2] = ztmp
          ctmp = carr(i,j)
          carr(i,j) = carr(i,j)[im1,im2]
          carr(i,j)[im1,im2] = ctmp
          ltmp = larr(i,j)
          larr(i,j) = larr(i,j)[im1,im2]
          larr(i,j)[im1,im2] = ltmp
       end do
    end do

end subroutine subext
