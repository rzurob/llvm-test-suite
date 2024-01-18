!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : UNPACK intrinsic
!*
!* DESCRIPTION                : langlvl message
!* ===================================================================

implicit none

integer :: i, j

logical, parameter :: T=.true., F=.false.
logical, parameter :: msk(2,2)=reshape((/F,T,F,T,F,F,T,F,T/), (/2,2/))

integer(1), parameter :: v1(4)=(/5,6,7,8/)
integer(1), parameter :: fld1(2,2)=reshape((/-1,-2,-3,-4/),(/2,2/))
integer(1), dimension(2,2) :: res5=unpack(v1, msk, fld1)

integer(2), parameter :: v2(4)=(/5,6,7,8/)
integer(2), parameter :: fld2(2,2)=reshape((/-1,-2,-3,-4/),(/2,2/))
integer(2), dimension(2,2) :: res0=unpack(v2, msk, fld2)

integer(4), parameter :: v4(4)=(/5,6,7,8/)
integer(4), parameter :: fld4(2,2)=reshape((/-1,-2,-3,-4/),(/2,2/))
integer(4), dimension(2,2) :: res1=unpack(v4, msk, fld4)

integer(8), parameter :: v3(4)=(/1,2,3,4/)
integer(8), parameter :: fld8(2,2)=reshape((/-1,-2,-3,-4/), (/2,2/))
integer(8), dimension(2,2) :: res3=unpack(v3, mask=msk, field=fld8)

logical(1), parameter :: u1(4)=(/T,F,F,T/)
logical(1), parameter :: fldL1(2,2)=reshape((/T,F,F,T/),(/2,2/))
logical(1), dimension(2,2) :: resL5=unpack(u1, msk, fldL1)

logical(2), parameter :: u2(4)=(/T,F,F,T/)
logical(2), parameter :: fldL2(2,2)=reshape((/T,F,F,T/),(/2,2/))
logical(2), dimension(2,2) :: resL0=unpack(u2, msk, fldL2)

logical(4), parameter :: u4(4)=(/T,F,F,T/)
logical(4), parameter :: fldL4(2,2)=reshape((/T,F,F,T/),(/2,2/))
logical(4), dimension(2,2) :: resL1=unpack(u4, msk, fldL4)

logical(8), parameter :: u3(4)=(/T,F,F,T/)
logical(8), parameter :: fldL8(2,2)=reshape((/T,F,F,T/), (/2,2/))
logical(8), dimension(2,2) :: resL3=unpack(u3, mask=msk, field=fldL8)

real(4), parameter :: r4(4)=(/5,6,7,8/)
real(4), parameter :: fldR4(2,2)=reshape((/-1,-2,-3,-4/),(/2,2/))
real(4), dimension(2,2) :: resR1=unpack(r4, msk, fldR4)

real(8), parameter :: r8(4)=(/1,2,3,4/)
real(8), parameter :: fldR8(2,2)=reshape((/-1,-2,-3,-4/), (/2,2/))
real(8), dimension(2,2) :: resR3=unpack(r8, mask=msk, field=fldR8)

real(16), parameter :: r16(4)=(/1,2,3,4/)
real(16), parameter :: fldR16(2,2)=reshape((/-1,-2,-3,-4/), (/2,2/))
real(16), dimension(2,2) :: resR16=unpack(r16, mask=msk, field=fldR16)

complex(4), parameter :: c4(4)=(/(5.,6.),(6.,7.),(7.,8.),(8.,9.)/)
complex(4), parameter :: fldC4(2,2)=reshape((/& 
 & cmplx(-1.0),cmplx(-2.0),cmplx(-3),cmplx(-4)/),(/2,2/))
complex(4), dimension(2,2) :: resC1=unpack(c4, msk, fldC4)
 
complex(8), parameter :: c8(4)=(/(1.,1.),(2.,2.),(3.,3.),(4.,4.)/)
complex(8), parameter :: fldC8(2,2)=reshape((/& 
 & cmplx(-1.0),cmplx(-2.0),cmplx(-3),cmplx(-4)/), (/2,2/))
complex(8), dimension(2,2) :: resC3=unpack(c8, mask=msk, field=fldC8)

complex(16), parameter :: c16(4)=(/(1.q0,0.q0),(2.0q0,0.q0),(3.q0,0.q0),(4.q0,0.q0)/)
complex(16), parameter :: fldC16(2,2)=reshape((/& 
 & cmplx(-1.0),cmplx(-2.0),cmplx(-3),cmplx(-4)/), (/2,2/))
complex(16), dimension(2,2) :: resC16=unpack(c16, mask=msk, field=fldC16)



end
