!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : cdaasModule
!*
!*  DATE                       : 2010-09-30
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray dummy arguments - assumed size
!*  SECONDARY FUNCTIONS TESTED : coarray variables from a module
!*  ADAPTED FROM               : cdaasBasicCopy
!*
!*  DESCRIPTION
!*
!*  Make sure coarray variables defined in an array can be accessed and mutated
!*  when passed in as assumed-size.  Also, try the updates in three different
!*  kinds of subroutine: module, internal, and external.  The subroutine(s) now
!*  swap rather than copying.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module cdaasConstantsMod
    integer, parameter :: SZ1 = 2, SZ2 = 3, SZ3 = 2
    integer, parameter :: P = 1, Q = 2
end module cdaasConstantsMod


module cdaasModuleMod
    use :: cdaasConstantsMod
    integer, save :: ico(SZ1,SZ2,SZ3)[*] = 0
    real(8), save :: rco(SZ1,SZ2,SZ3)[*] = 0.0d0
    complex(8), save :: zco(SZ1,SZ2,SZ3)[*] = (0.0d0,0.0d0)
    character(4), save :: cco(SZ1,SZ2,SZ3)[*] = ''
    logical(1), save :: lco(SZ1,SZ2,SZ3)[*] = .false.

  contains

    subroutine submod(iarr, rarr, zarr, carr, larr, n, im)
        integer :: n, im
        integer :: iarr(SZ1*SZ2,*)[*], itmp
        real(8) :: rarr(SZ1*SZ2,*)[*], rtmp
        complex(8) :: zarr(SZ1*SZ2,*)[*], ztmp
        character(4) :: carr(SZ1*SZ2,*)[*], ctmp
        logical(1) :: larr(SZ1*SZ2,*)[*], ltmp
        integer :: i, j, k

        do i = 1, SZ1*SZ2
           do k = 1, n
              itmp = iarr(i,k)
              iarr(i,k) = iarr(i,k)[im]
              iarr(i,k)[im] = itmp

              rtmp = rarr(i,k)
              rarr(i,k) = rarr(i,k)[im]
              rarr(i,k)[im] = rtmp

              ztmp = zarr(i,k)
              zarr(i,k) = zarr(i,k)[im]
              zarr(i,k)[im] = ztmp

              ctmp = carr(i,k)
              carr(i,k) = carr(i,k)[im]
              carr(i,k)[im] = ctmp

              ltmp = larr(i,k)
              larr(i,k) = larr(i,k)[im]
              larr(i,k)[im] = ltmp
           end do
        end do

    end subroutine submod

end module cdaasModuleMod

program cdaasModule
    use :: cdaasModuleMod
    implicit none

    interface
		subroutine subext(iarr, rarr, zarr, carr, larr, n, im)
		    integer :: n, im
		    integer :: iarr(SZ1,SZ2,*)[*]
		    real(8) :: rarr(SZ1,SZ2,*)[*]
		    complex(8) :: zarr(SZ1,SZ2,*)[*]
		    character(4) :: carr(SZ1,SZ2,*)[*]
		    logical(1) :: larr(SZ1,SZ2,*)[*]
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

    sync all
    if (curImage == P) then
      call submod(ico, rco, zco, cco, lco, SZ3, Q)
    end if

    sync all
    if (curImage == Q) then
      print *, ico
      print *, rco
      print *, zco
      print *, cco
      print *, lco
    end if
    sync all
    if (curImage == P) then
      call subext(ico, rco, zco, cco, lco, SZ3, Q)
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

end program cdaasModule


subroutine subext(iarr, rarr, zarr, carr, larr, n, im)
    use :: cdaasConstantsMod
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

    do i = 1, SZ2
       do j = 1, SZ1
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

end subroutine subext
