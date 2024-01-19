!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-09-29
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray dummy arguments - assumed size
!*  SECONDARY FUNCTIONS TESTED : arguments passed in as arguments
!*  ADAPTED FROM               : cdaasModule
!*
!*  DESCRIPTION
!*
!*  Call subroutines in several layers, passing coarrays on from one layer to
!*  the next so that we test the effective argument (which resides in a module,
!*  although the routine which ultimately alters it does not have direct access
!*  to the module).
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module cdaasConstantsMod
    integer, parameter :: SZ1 = 2, SZ2 = 3, SZ3 = 2
    integer, parameter :: P = 1, Q = 2
end module cdaasConstantsMod


module cdaasEffectiveMod
    use :: cdaasConstantsMod
    integer, save :: ico(SZ1,SZ2,SZ3)[*] = 0
    real(8), save :: rco(SZ1,SZ2,SZ3)[*] = 0.0d0
    complex(8), save :: zco(SZ1,SZ2,SZ3)[*] = (0.0d0,0.0d0)
    character(4), save :: cco(SZ1,SZ2,SZ3)[*] = ''
    logical(1), save :: lco(SZ1,SZ2,SZ3)[*] = .false.

  contains

    subroutine doSums_mod(iarr, rarr, zarr, carr, larr, n, im)
        integer :: n, im
        integer :: iarr(SZ1*SZ2,*)[*]
        real(8) :: rarr(SZ1*SZ2,*)[*]
        complex(8) :: zarr(SZ1*SZ2,*)[*]
        character(4) :: carr(SZ1*SZ2,*)[*]
        logical(1) :: larr(SZ1*SZ2,*)[*]

	    interface
	        subroutine doSums_ext(iarr, rarr, zarr, carr, larr, n, im)
	            integer :: n, im
	            integer :: iarr(SZ2,SZ1,*)[*]
	            real(8) :: rarr(SZ2,SZ1,*)[*]
	            complex(8) :: zarr(SZ2,SZ1,*)[*]
	            character(4) :: carr(SZ2,SZ1,*)[*]
	            logical(1) :: larr(SZ2,SZ1,*)[*]
	        end subroutine doSums_ext
	    end interface

        call doSums_ext(iarr, rarr, zarr, carr, larr, n, im)

    end subroutine doSums_mod

end module cdaasEffectiveMod

program cdaasEffective
    use :: cdaasEffectiveMod
    implicit none
    logical*4 :: precision_r8, precision_x16
    external  :: precision_r8, precision_x16

    interface
      character(4) function csum2(arr, n)
        integer :: n
        character(4) :: arr(SZ2,SZ1,*)
      end function csum2
      logical(1) function lsum2(arr, n)
        integer :: n
        logical(1) :: arr(SZ2,SZ1,*)
      end function lsum2
    end interface

    integer :: ia(SZ1*SZ2*SZ3)
    real(8) :: ra(SZ1*SZ2*SZ3)
    integer :: i, j, curImage, nImages

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
      call doSums(ico, rco, zco, cco, lco, SZ3, nImages)
    end if

    sync all
    if (curImage == Q) then
       do i = 1, nImages
          print *, 'checking', i
          ia = [((i-1) * 20 + i, i=1,SZ1*SZ2*SZ3)]
          ra = 1.0d0 / ia
          if (ico(1,1,1)[i] /= sum(ia)) error stop 2
          if (.not.precision_r8(rco(1,1,1)[i], sum(ra))) error stop 3
          if (.not.precision_x16(zco(1,1,1)[i], sum(cmplx(real(i,kind(ra)), ra, kind(ra))))) error stop 4
          if (csum2(achar(iachar('A')+i-1) // reshape([(repeat(achar(iachar('a')+ i),3), i=0,SZ1*SZ2*SZ3-1)], [SZ1,SZ2,SZ3]), SZ3) &
               /= cco(1,1,1)[i]) error stop 5
          if (lsum2(reshape(logical([(mod(j,4) == i, j=1,SZ1*SZ2*SZ3)],1), [SZ1,SZ2,SZ3]), SZ3) .neqv. lco(1,1,1)[i]) error stop 6
          print *, ico(1,1,1)[i], rco(1,1,1)[i], zco(1,1,1)[i], cco(1,1,1)[i], lco(1,1,1)[i]
       end do
    end if

    print *, 'done'

  contains

    subroutine doSums(iarr, rarr, zarr, carr, larr, n, im)
        integer :: n, im
        integer :: iarr(SZ1,SZ2,*)[*]
        real(8) :: rarr(SZ1,SZ2,*)[*]
        complex(8) :: zarr(SZ1,SZ2,*)[*]
        character(4) :: carr(SZ1,SZ2,*)[*]
        logical(1) :: larr(SZ1,SZ2,*)[*]

        call doSums_mod(iarr, rarr, zarr, carr, larr, n, im)

    end subroutine doSums

end program cdaasEffective


subroutine doSums_ext(iarr, rarr, zarr, carr, larr, n, m)
    use :: cdaasConstantsMod
    integer :: n, im
    integer :: iarr(SZ2,SZ1,*)[*]
    real(8) :: rarr(SZ2,SZ1,*)[*]
    complex(8) :: zarr(SZ2,SZ1,*)[*]
    character(4) :: carr(SZ2,SZ1,*)[*]
    logical(1) :: larr(SZ2,SZ1,*)[*]
    integer :: i, j, k

    interface
      integer function isum(arr, n, im)
        integer :: n, im
        integer :: arr(SZ2,SZ1,*)[*]
      end function isum
      complex(8) function zsum(arr, n, im)
        integer :: n, im
        complex(8) :: arr(SZ2,SZ1,*)[*]
      end function zsum
      real(8) function rsum(arr, n, im)
        integer :: n, im
        real(8) :: arr(SZ2,SZ1,*)[*]
      end function rsum
      character(4) function csum(arr, n, im)
        integer :: n, im
        character(4) :: arr(SZ2,SZ1,*)[*]
      end function csum
      logical(1) function lsum(arr, n, im)
        integer :: n, im
        logical(1) :: arr(SZ2,SZ1,*)[*]
      end function lsum
    end interface

    do l = 1, m
       iarr(1,1,1)[l] = isum(iarr, n, l)
       rarr(1,1,1)[l] = rsum(rarr, n, l)
       zarr(1,1,1)[l] = zsum(zarr, n, l)
       carr(1,1,1)[l] = csum(carr, n, l)
       larr(1,1,1)[l] = lsum(larr, n, l)
    end do

end subroutine doSums_ext


integer function isum(arr, n, im)
    use :: cdaasConstantsMod
    integer :: n, im
    integer :: arr(SZ2,SZ1,*)[*]
    integer :: i, j, k

    isum = 0

    do i = 1, SZ2
       do j = 1, SZ1
          do k = 1, n
             isum = isum + arr(i,j,k)[im]
          end do
       end do
    end do

end function isum


complex(8) function zsum(arr, n, im)
    use :: cdaasConstantsMod
    integer :: n, im
    complex(8) :: arr(SZ2,SZ1,*)[*]
    integer :: i, j, k

    zsum = 0

    do i = 1, SZ2
       do j = 1, SZ1
          do k = 1, n
             zsum = zsum + arr(i,j,k)[im]
          end do
       end do
    end do

end function zsum



real(8) function rsum(arr, n, im)
    use :: cdaasConstantsMod
    integer :: n, im
    real(8) :: arr(SZ2,SZ1,*)[*]
    integer :: i, j, k

    rsum = 0

    do i = 1, SZ2
       do j = 1, SZ1
          do k = 1, n
             rsum = rsum + arr(i,j,k)[im]
          end do
       end do
    end do

end function rsum


character(4) function csum(arr, n, im)
    use :: cdaasConstantsMod
    integer :: n, im
    character(4) :: arr(SZ2,SZ1,*)[*]
    integer :: i, j, k, ihash

    ihash = 0

    do i = 1, SZ2
       do j = 1, SZ1
          do k = 1, n
             ihash = ihash + iachar(arr(i,j,k)(2:2)[im]) - 32
          end do
       end do
    end do

    csum = repeat(achar(mod(ihash,127-32)+32), 4)

end function csum

character(4) function csum2(arr, n)
    use :: cdaasConstantsMod
    integer :: n
    character(4) :: arr(SZ2,SZ1,*)
    integer :: i, j, k, ihash

    ihash = 0

    do i = 1, SZ2
       do j = 1, SZ1
          do k = 1, n
             ihash = ihash + iachar(arr(i,j,k)(2:2)) - 32
          end do
       end do
    end do

    csum2 = repeat(achar(mod(ihash,127-32)+32), 4)

end function csum2


logical(1) function lsum(arr, n, im)
    use :: cdaasConstantsMod
    integer :: n, im
    logical(1) :: arr(SZ2,SZ1,*)[*]
    integer :: i, j, k

    lsum = .false.

    do i = 1, SZ2
       do j = 1, SZ1
          do k = 1, n
             lsum = lsum .neqv. arr(i,j,k)[im]
          end do
       end do
    end do

end function lsum


logical(1) function lsum2(arr, n)
    use :: cdaasConstantsMod
    integer :: n
    logical(1) :: arr(SZ2,SZ1,*)
    integer :: i, j, k

    lsum2 = .false.

    do i = 1, SZ2
       do j = 1, SZ1
          do k = 1, n
             lsum2 = lsum2 .neqv. arr(i,j,k)
          end do
       end do
    end do

end function lsum2
