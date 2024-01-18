!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : MATMUL intrinsic
!*
!* DESCRIPTION                : langlvl message
!* ===================================================================

! integer type
integer(1), dimension(2,2), parameter :: A1=reshape((/(i,i=1,4)/),(/2,2/))
integer(1), dimension(2,2), parameter :: B1=reshape((/(i,i=1,4)/),(/2,2/))
integer(1), dimension(2,2) :: res1=matmul(A1,B1)

integer(2), dimension(2,2), parameter :: A2=reshape((/(i,i=1,4)/),(/2,2/))
integer(2), dimension(2,2), parameter :: B2=reshape((/(i,i=1,4)/),(/2,2/))
integer(2), dimension(2,2) :: res2=matmul(A2,B2)

integer(4), dimension(2,2), parameter :: A4=reshape((/(i,i=1,4)/),(/2,2/))
integer(4), dimension(2,2), parameter :: B4=reshape((/(i,i=1,4)/),(/2,2/))
integer(4), dimension(2,2) :: res4=matmul(A4,B4)

integer(8), dimension(2,2), parameter :: A8=reshape((/(i,i=1,4)/),(/2,2/))
integer(8), dimension(2,2), parameter :: B8=reshape((/(i,i=1,4)/),(/2,2/))
integer(8), dimension(2,2) :: res8=matmul(A8,B8)

! logical type
logical(1), dimension(2,2), parameter :: A1l=reshape((/(.true.,i=1,4)/),(/2,2/))
logical(1), dimension(2,2), parameter :: B1l=reshape((/(.true.,i=1,4)/),(/2,2/))
logical(1), dimension(2,2) :: res1l=matmul(A1l,B1l)

logical(2), dimension(2,2), parameter :: A2l=reshape((/(.true.,i=1,4)/),(/2,2/))
logical(2), dimension(2,2), parameter :: B2l=reshape((/(.true.,i=1,4)/),(/2,2/))
logical(2), dimension(2,2) :: res2l=matmul(A2l,B2l)

logical(4), dimension(2,2), parameter :: A4l=reshape((/(.true.,i=1,4)/),(/2,2/))
logical(4), dimension(2,2), parameter :: B4l=reshape((/(.true.,i=1,4)/),(/2,2/))
logical(4), dimension(2,2) :: res4l=matmul(A4l,B4l)

logical(8), dimension(2,2), parameter :: A8l=reshape((/(.true.,i=1,4)/),(/2,2/))
logical(8), dimension(2,2), parameter :: B8l=reshape((/(.true.,i=1,4)/),(/2,2/))
logical(8), dimension(2,2) :: res8l=matmul(A8l,B8l)

! real type
real(4), dimension(2,2), parameter :: A4r=reshape((/(1.0,i=1,4)/),(/2,2/))
real(4), dimension(2,2), parameter :: B4r=reshape((/(1.0,i=1,4)/),(/2,2/))
real(4), dimension(2,2) :: res4r=matmul(A4r,B4r)

real(8), dimension(2,2), parameter :: A8r=reshape((/(1.0,i=1,4)/),(/2,2/))
real(8), dimension(2,2), parameter :: B8r=reshape((/(1.0,i=1,4)/),(/2,2/))
real(8), dimension(2,2) :: res8r=matmul(A8r,B8r)

real(16), dimension(2,2), parameter :: A16r=reshape((/(1.0,i=1,4)/),(/2,2/))
real(16), dimension(2,2), parameter :: B16r=reshape((/(1.0,i=1,4)/),(/2,2/))
real(16), dimension(2,2) :: res16r=matmul(A16r,B16r)

! complex type
complex(4), dimension(2,2), parameter :: A4c=reshape((/((1.0,1.0),i=1,4)/),(/2,2/))
complex(4), dimension(2,2), parameter :: B4c=reshape((/((1.0,1.0),i=1,4)/),(/2,2/))
complex(4), dimension(2,2) :: res4c=matmul(A4c,B4c)

complex(8), dimension(2,2), parameter :: A8c=reshape((/((1.0,1.0),i=1,4)/),(/2,2/))
complex(8), dimension(2,2), parameter :: B8c=reshape((/((1.0,1.0),i=1,4)/),(/2,2/))
complex(8), dimension(2,2) :: res8c=matmul(A8c,B8c)

complex(16), dimension(2,2), parameter :: A16c=reshape((/((1.0,1.0),i=1,4)/),(/2,2/))
complex(16), dimension(2,2), parameter :: B16c=reshape((/((1.0,1.0),i=1,4)/),(/2,2/))
complex(16), dimension(2,2) :: res16c=matmul(A16c,B16c)
      
end
