!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : RESHAPE intrinsic
!*
!* DESCRIPTION                : langlvl message
!* ===================================================================

implicit none

character(3) :: c(1,3)=reshape((/'abc','abd',' a '/), (/1,3/))

integer(1) :: i1(2,2)=reshape((/1,2,3,4/),(/2,2/))
integer(2) :: i2(2,2)=reshape((/1,2,3,4/),(/2,2/))
integer(4) :: i4(2,2)=reshape((/1,2,3,4/),(/2,2/))
integer(8) :: i8(2,2)=reshape((/1,2,3,4/),(/2,2/))

logical(1) :: l1(2,2)=reshape((/.true.,.true.,.false.,.false./),(/2,2/))
logical(2) :: l2(2,2)=reshape((/.true.,.true.,.false.,.false./),(/2,2/))
logical(4) :: l4(2,2)=reshape((/.true.,.true.,.false.,.false./),(/2,2/))
logical(8) :: l8(2,2)=reshape((/.true.,.true.,.false.,.false./),(/2,2/))

real(4) :: r4(2,2)=reshape((/1.0,2.0,3.0,4.0/),(/2,2/))
real(8) :: r8(2,2)=reshape((/1.0,2.0,3.0,4.0/),(/2,2/))
real(16) :: r16(2,2)=reshape((/1.0,2.0,3.0,4.0/),(/2,2/))

complex(4) :: c4(2,1)=reshape((/(1.0,2.0),(3.0,4.0)/),(/2,1/))
complex(8) :: c8(2,1)=reshape((/(1.0,2.0),(3.0,4.0)/),(/2,1/))
complex(16) :: c16(2,1)=reshape((/(1.0,2.0),(3.0,4.0)/),(/2,1/))

end
