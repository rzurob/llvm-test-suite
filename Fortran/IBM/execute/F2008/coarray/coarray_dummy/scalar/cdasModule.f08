!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-11-18
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray dummy arguments - scalar
!*  SECONDARY FUNCTIONS TESTED : coarray variables from a module
!*  ADAPTED FROM               : cdaasModule <- cdaasBasicCopy
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
    integer, parameter :: P = 1, Q = 2
end module cdaasConstantsMod


module cdasModuleMod
  use :: cdaasConstantsMod
  integer, save :: ico[*] = 0
  real(8), save :: rco[*] = 0.0d0
  complex(8), save :: zco[*] = (0.0d0,0.0d0)
  character(4), save :: cco[*] = ''
  logical(1), save :: lco[*] = .false.

contains

  subroutine submod(iarr, rarr, zarr, carr, larr, im)
    integer :: im
    integer :: iarr[*], itmp
    real(8) :: rarr[*], rtmp
    complex(8) :: zarr[*], ztmp
    character(4) :: carr[*], ctmp
    logical(1) :: larr[*], ltmp

    itmp = iarr; iarr = iarr[im]; iarr[im] = itmp
    rtmp = rarr; rarr = rarr[im]; rarr[im] = rtmp
    ztmp = zarr; zarr = zarr[im]; zarr[im] = ztmp
    ctmp = carr; carr = carr[im]; carr[im] = ctmp
    ltmp = larr; larr = larr[im]; larr[im] = ltmp
  end subroutine submod

end module cdasModuleMod

program cdasModule
  use :: cdasModuleMod
  implicit none

  interface
     subroutine subext(iarr, rarr, zarr, carr, larr, im)
       integer :: im
       integer :: iarr[*]
       real(8) :: rarr[*]
       complex(8) :: zarr[*]
       character(4) :: carr[*]
       logical(1) :: larr[*]
     end subroutine subext
  end interface

  integer :: curImage, nImages

  curImage = this_image()
  nImages  = num_images()

  if (nImages < max(P,Q)) error stop 2

  ico = -(curImage-3)
  rco = 1.0d0 / ico
  zco = cmplx(real(curImage,kind(zco)), rco, kind(zco))
  cco = repeat(achar(iachar('A')+curImage), len(cco))
  lco = mod(curImage,2) == 0

  ! this is the boring way to test it, but it makes the verification easier:
  ! sync, make a change, sync, output it, sync, make another change, sync, output it, etc.
  sync all
  if (curImage == P) then
     call sub(ico, rco, zco, cco, lco, Q)
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
     call submod(ico, rco, zco, cco, lco, Q)
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
     call subext(ico, rco, zco, cco, lco, Q)
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

  subroutine sub(iarr, rarr, zarr, carr, larr, im)
    integer :: im
    integer :: iarr[*]
    real(8) :: rarr[*]
    complex(8) :: zarr[*]
    character(4) :: carr[*]
    logical(1) :: larr[*]
    integer :: itmp
    real(8) :: rtmp
    complex(8) :: ztmp
    character(4) :: ctmp
    logical(1) :: ltmp

    itmp = iarr; iarr = iarr[im]; iarr[im] = itmp
    rtmp = rarr; rarr = rarr[im]; rarr[im] = rtmp
    ztmp = zarr; zarr = zarr[im]; zarr[im] = ztmp
    ctmp = carr; carr = carr[im]; carr[im] = ctmp
    ltmp = larr; larr = larr[im]; larr[im] = ltmp
  end subroutine sub

end program cdasModule


subroutine subext(iarr, rarr, zarr, carr, larr, im)
  use :: cdaasConstantsMod
  integer :: im
  integer :: iarr[*]
  real(8) :: rarr[*]
  complex(8) :: zarr[*]
  character(4) :: carr[*]
  logical(1) :: larr[*]
  integer :: itmp
  real(8) :: rtmp
  complex(8) :: ztmp
  character(4) :: ctmp
  logical(1) :: ltmp

  itmp = iarr; iarr = iarr[im]; iarr[im] = itmp
  rtmp = rarr; rarr = rarr[im]; rarr[im] = rtmp
  ztmp = zarr; zarr = zarr[im]; zarr[im] = ztmp
  ctmp = carr; carr = carr[im]; carr[im] = ctmp
  ltmp = larr; larr = larr[im]; larr[im] = ltmp
end subroutine subext
