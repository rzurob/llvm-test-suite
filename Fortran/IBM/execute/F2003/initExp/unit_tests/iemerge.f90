!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Initialization Expression
!*
!*  PROGRAMMER                 : James Ren
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Diagnostic test
!*
!*  DRIVER STANZA              : xlf90/95
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTIONS               : Testing SCALE intrinsic function
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

implicit none

integer i, j
logical, parameter :: T=.true., F=.false.
logical, parameter :: mask(3,3)=reshape((/F,T,F,T,F,T,F,T,F/), (/3,3/))
integer, parameter :: ts(3,3)=reshape((/-1,-2,-3,-4,-5,-6,-7,-8,-9/),(/3,3/))
integer, parameter :: fs(3,3)=reshape((/1,2,3,4,5,6,7,8,9/),(/3,3/))
complex, parameter :: tc(3,3)=reshape((/(-1,-1),(-2,-2),(-3,-3),(-4,-4),(-5,-5),(-6,-6),(-7,-7),(-8,-8),(-9,-9)/),(/3,3/))
complex, parameter :: fc(3,3)=reshape((/(1,1),(2,2),(3,3),(4,4),(5,5),(6,6),(7,7),(8,8),(9,9)/),(/3,3/))

integer*1, parameter :: r1(3,3)=merge(ts, fs, mask)
integer*2, parameter :: r2(3,3)=merge(ts, fs, mask)
integer*4, parameter :: r4(3,3)=merge(ts, fs, mask)
integer*8, parameter :: r8(3,3)=merge(ts, fs, mask)

real*4, parameter, dimension(3,3) :: rd4 = merge(ts, fs, mask)
real*8, parameter, dimension(3,3) :: rd8 = merge(ts, fs, mask)
real*16, parameter, dimension(3,3) :: rd16 = merge(ts, fs, mask)

complex*4, parameter, dimension(3,3) :: cd4 = merge(tc, fc, mask)
complex*8, parameter, dimension(3,3) :: cd8 = merge(tc, fc, mask)
complex*16, parameter, dimension(3,3) :: cd16 = merge(tc, fc, mask)

integer, dimension(3,3) :: resi
real*4, dimension(3,3) :: resr4
real*8, dimension(3,3) :: resr8
real*16, dimension(3,3) :: resr16
complex*4, dimension(3,3) :: resc4
complex*8, dimension(3,3) :: resc8
complex*16, dimension(3,3) :: resc16

resi = merge(ts, fs, mask)
resr4 = merge(ts, fs, mask)
resr8 = merge(ts, fs, mask)
resr16 = merge(ts, fs, mask)
resc4 = merge(tc, fc, mask)
resc8 = merge(tc, fc, mask)
resc16 = merge(tc, fc, mask)

do i = 1, 3
   do j =1, 3
      if (r1(i, j) /= resi(i, j)) error stop 1
      if (r2(i, j) /= resi(i, j)) error stop 2
      if (r4(i, j) /= resi(i, j)) error stop 3
      if (r8(i, j) /= resi(i, j)) error stop 4
      if (rd4(i, j) /= resr4(i, j)) error stop 5
      if (rd8(i, j) /= resr8(i, j)) error stop 6
      if (rd16(i, j) /= resr16(i, j)) error stop 7
      if (cd4(i, j) /= resc4(i, j)) error stop 8
      if (cd8(i, j) /= resc8(i, j)) error stop 9
      if (cd16(i, j) /= resc16(i, j)) error stop 10
   end do
end do

end
