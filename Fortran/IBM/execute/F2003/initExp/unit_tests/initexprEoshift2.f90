!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : EOSHIFT intrinsic
!*
!* DESCRIPTION                : real type
!* ===================================================================

implicit none
integer i, j, k
real(4), parameter, dimension(8) :: c0=(/((2.0E0*i+1.0E0),i=-2,5)/)
real(4), dimension(8) :: c1res, c1=eoshift(c0, 3)

real(4), parameter, dimension(2,3) :: c2=reshape(c0, (/2,3/))
real(4), dimension(2,3) :: c2res, c2a=eoshift(c2, shift=(/1,-1,0/), dim=1)

real(4), parameter, dimension(2,2,2) :: c3=reshape(c0, (/2,2,2/))
real(4), dimension(2,2,2) :: c3res, c3a=eoshift(c3, &
 & shift=reshape((/-1,1,1,-1/),(/2,2/)), dim=2, &
 & boundary=(/-100.0,-200.0/))

logical  precision_r4, precision_r8, precision_r16

real(8), parameter, dimension(18) :: d0=(/(2.0D0*i**2,i=1001,1120,7)/)
real(8), dimension(18) :: d1res, d1=eoshift(d0, -3)

real(8), parameter, dimension(6,3) :: d2=reshape(d0, (/6,3/))
real(8), dimension(6,3) :: d2res, d2a=eoshift(d2, shift=(/1,-1,0/), dim=1)

real(8), parameter, dimension(3,2,3) :: d3=reshape(d0, (/3,2,3/))
real(8), dimension(3,2,3) :: d3res, d3a=eoshift(d3, &
  & shift=reshape((/0,-1,1,1,-1,0,-2,2,1/),(/3,3/)), dim=2, &
  & boundary=reshape((/-1.0D0,-2.0D-1,-1.0D0,-2.0D-1,-1.0D0,-2.0D-1, &
  & -1.0D0,-2.0D-1,-1.0D0/),(/3,3/)))

real(16), parameter, dimension(151) :: e0=(/(7.1*i,i=1,151)/)
real(16), dimension(151) :: e1=eoshift(e0, shift=-1)

c1res = eoshift(c0,3)
if (.not. all(c1 .eq. c1res)) then
  stop 1
endif

c2res = eoshift(c2, shift=(/1,-1,0/), dim=1)
if (.not. all(c2a .eq. c2res)) then
  stop 2
endif

c3res = eoshift(c3, shift=reshape((/-1,1,1,-1/),(/2,2/)), dim=2, &
 & boundary=(/-100.0,-200.0/))
if (.not. all(c3a .eq. c3res)) then
  stop 3
endif

d1res = eoshift(d0, -3)
if (.not. all(d1 .eq. d1res)) then
  stop 4
endif

d2res = eoshift(d2, shift=(/1,-1,0/), dim=1)
if (.not. all(d2a .eq. d2res)) then
  stop 5
endif

d3res = eoshift(d3, shift=reshape((/0,-1,1,1,-1,0,-2,2,1/),(/3,3/)), &
  & dim=2, boundary=reshape((/-1.0D0,-2.0D-1,-1.0D0,-2.0D-1,-1.0D0, &
  & -2.0D-1, -1.0D0,-2.0D-1,-1.0D0/), (/3,3/)))

do i=1,3
  do j=1,2
    do k=1,3
      if ( .not. precision_r8(d3res(i,j,k), d3a(i,j,k))) then
        print *, i, j, k, d3res(i,j,k), d3a(i,j,k)
        stop 6
      endif
    enddo
  enddo
enddo

if (.not. all(e1 .eq. eoshift(e0, shift=-1))) stop 7

end
