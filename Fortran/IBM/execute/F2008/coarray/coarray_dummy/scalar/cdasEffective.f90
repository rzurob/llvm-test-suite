!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : cdasEffective
!*
!*  DATE                       : 2010-11-18
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray dummy arguments - scalar
!*  SECONDARY FUNCTIONS TESTED : arguments passed in as arguments
!*  ADAPTED FROM               : cdaasEffective <- cdaasModule
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

module cdasConstantsMod
  integer, parameter :: P = 1, Q = 2
end module cdasConstantsMod


module cdasEffectiveMod
  use :: cdasConstantsMod
  integer, save :: ico[*] = 0
  real(8), save :: rco[*] = 0.0d0
  complex(8), save :: zco[*] = (0.0d0,0.0d0)
  character(4), save :: cco[*] = ''
  logical(1), save :: lco[*] = .false.

contains

  subroutine doFuns_mod(iarg, rarg, zarg, carg, larg, im)
    integer :: im
    integer :: iarg[*]
    real(8) :: rarg[*]
    complex(8) :: zarg[*]
    character(4) :: carg[*]
    logical(1) :: larg[*]

    interface
       subroutine doFuns_ext(iarg, rarg, zarg, carg, larg, im)
         integer :: im
         integer :: iarg[*]
         real(8) :: rarg[*]
         complex(8) :: zarg[*]
         character(4) :: carg[*]
         logical(1) :: larg[*]
       end subroutine doFuns_ext
    end interface

    call doFuns_ext(iarg, rarg, zarg, carg, larg, im)

  end subroutine doFuns_mod

end module cdasEffectiveMod

program cdasEffective
  use :: cdasEffectiveMod
  implicit none
  logical*4 :: precision_r8, precision_x16
  external  :: precision_r8, precision_x16

  integer :: ia
  real(8) :: ra
  integer :: i, curImage, nImages

  curImage = this_image()
  nImages  = num_images()

  if (nImages < max(P,Q)) error stop 2

  ico = (curImage-1) * 20 + 1
  rco = 1.0d0 / ico
  zco = cmplx(real(curImage,kind(zco)), rco, kind(zco))
  cco = repeat(achar(iachar('A')+curImage-1), len(cco))
  lco = curImage == 1

  ! 1: I= 1  R=1.0000  Z=(1 1)     C=AAAA  L=T
  ! 2: I=21  R=0.0476  Z=(2 1/21)  C=BBBB  L=F
  ! 3: I=41  R=0.0244  Z=(3 1/41)  C=CCCC  L=F

  sync all
  if (curImage == P) then
     call doFuns(ico, rco, zco, cco, lco, nImages)
  end if

  ! 1: I= 11  R=  1.00  Z=(5    -1)     C=BBBB   L=F
  ! 2: I=212  R= 42.02  Z=(5.05 -4.95)  C=CCCC   L=T
  ! 3: I=413  R=122.95  Z=(7.02 -6.98)  C=DDDD   L=T

  ! now 1 calls doFuns_ext, which calls ifun(iarg,1)...(iarg,n), then rfun..., etc.
  sync all
  if (curImage == Q) then
     do i = 1, nImages
        print *, 'checking', i
        ia = (i-1) * 20 + 1
        ra = 1.0d0 / ia
        if (ico[i] /= (ia*10+i)) error stop 2
        if (.not.precision_r8(rco[i], i/ra)) error stop 3
        if (.not.precision_x16(zco[i], cmplx(ra+i+1,i+1)*(1,-1))) error stop 4
        if (repeat(achar(iachar('A')+i),4) /= cco[i]) error stop 5
        if (((mod(i,2)==0).and.(i>1)) .neqv. lco[i]) error stop 6
        print *, ico[i], rco[i], zco[i], cco[i], lco[i]
     end do
  end if

  print *, 'done'

contains

  subroutine doFuns(iarg, rarg, zarg, carg, larg, im)
    integer :: im
    integer :: iarg[*]
    real(8) :: rarg[*]
    complex(8) :: zarg[*]
    character(4) :: carg[*]
    logical(1) :: larg[*]

    call doFuns_mod(iarg, rarg, zarg, carg, larg, im)

  end subroutine doFuns

end program cdasEffective


subroutine doFuns_ext(iarg, rarg, zarg, carg, larg, im)
  use :: cdasConstantsMod
  integer :: im
  integer :: iarg[*]
  real(8) :: rarg[*]
  complex(8) :: zarg[*]
  character(4) :: carg[*]
  logical(1) :: larg[*]

  interface
     integer function ifun(arg, im)
       integer :: im
       integer :: arg[*]
     end function ifun
     complex(8) function zfun(arg, im)
       integer :: im
       complex(8) :: arg[*]
     end function zfun
     real(8) function rfun(arg, im)
       integer :: im
       real(8) :: arg[*]
     end function rfun
     character(4) function cfun(arg, im)
       integer :: im
       character(4) :: arg[*]
     end function cfun
     logical(1) function lfun(arg, im)
       integer :: im
       logical(1) :: arg[*]
     end function lfun
  end interface

  do l = 1, im
     iarg[l] = ifun(iarg, l)
     rarg[l] = rfun(rarg, l)
     zarg[l] = zfun(zarg, l)
     carg[l] = cfun(carg, l)
     larg[l] = lfun(larg, l)
  end do

end subroutine doFuns_ext


integer function ifun(arg, im)
  use :: cdasConstantsMod
  integer :: im
  integer :: arg[*]

  ifun = arg[im] * 10 + im

end function ifun


real(8) function rfun(arg, im)
  use :: cdasConstantsMod
  integer :: im
  real(8) :: arg[*]

  rfun = im / arg[im]

end function rfun


complex(8) function zfun(arg, im)
  use :: cdasConstantsMod
  integer :: im
  complex(8) :: arg[*]

  zfun = (cmplx(aimag(arg[im])+im, real(arg[im])) + (1,1)) * (1,-1)

end function zfun



character(4) function cfun(arg, im)
  use :: cdasConstantsMod
  integer :: im
  character(4) :: arg[*]

  cfun = repeat(achar(mod(iachar(arg(2:2)[im]) - 32 + im, 127-32)+32), 4)

end function cfun


logical(1) function lfun(arg, im)
  use :: cdasConstantsMod
  integer :: im
  logical(1) :: arg[*]

  lfun = (mod(im,2)==0) .and. arg[im]

end function lfun
