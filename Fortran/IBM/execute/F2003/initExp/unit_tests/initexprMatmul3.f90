!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : MATMUL intrinsic
!*
!* DESCRIPTION                : logical type
!* ===================================================================

logical(1), dimension(2,1), parameter :: A1=.true.
logical(1), dimension(1,1), parameter :: B1=.false.

logical(2), dimension(11,13), parameter :: &
  &  A2=reshape((/(.true.,i=1,143,2), (.false.,i=2,143,2)/), (/11,13/))
logical(2), dimension(13,1), parameter :: &
  &  B2=reshape((/(.true. ,i=1,13,2), (.false.,i=2,13,2)/), (/13,1/))

logical(4), dimension(2,3) :: A4
logical(4), dimension(3,2) :: B4

logical(8), dimension(10,10), parameter :: &
  &  A8=reshape((/(.true.,i=101,200)/), (/10,10/))
logical(8), dimension(10,21), parameter :: &
  &  B8=reshape((/(.true.,i=1,210)/), (/10,21/))

parameter (A4=reshape((/.false.,.false.,.true.,.true.,.false.,.false./),(/2,3/)))
parameter (B4=reshape((/.true.,.false.,.false.,.true.,.false.,.true./),(/3,2/)))

logical(1), dimension(2,1) :: res1a=matmul(A1, B1), res1b
logical(2), dimension(11,1) :: res2a=matmul(A2, B2), res2b
logical(4), dimension(2,2) :: res4a=matmul(A4, B4), res4b
logical(8), dimension(10,21) :: res8a=matmul(A8, B8), res8b

res1b = matmul(A1,B1)
if (.not. all(res1a .eqv. res1b)) stop 1

res2b = matmul(A2,B2)
if (.not. all(res2a .eqv. res2b)) stop 2

res4b = matmul(A4,B4)
if (.not. all(res4a .eqv. res4b)) stop 3

res8b = matmul(A8,B8)
if (.not. all(res8a .eqv. res8b)) stop 4

end
