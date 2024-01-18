!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : TRANSPOSE intrinsic
!*
!* DESCRIPTION                : langlvl message
!* ===================================================================

implicit none

integer(1), parameter, dimension(2,2) :: A1=reshape((/0_1,2_1,7_1,-5_1/),(/2,2/))
integer(2), parameter, dimension(2,2) :: A2=reshape((/0_2,2_2,7_2,-5_2/),(/2,2/))
integer(4), parameter, dimension(2,2) :: A4=reshape((/0_4,2_4,7_4,-5_4/),(/2,2/))
integer(8), parameter, dimension(2,2) :: A8=reshape((/0_8,2_8,7_8,-5_8/),(/2,2/))

integer(1) :: A1res(2,2)=transpose(A1)
integer(2) :: A2res(2,2)=transpose(A2)
integer(4) :: A4res(2,2)=transpose(A4)
integer(8) :: A8res(2,2)=transpose(A8)

logical(1), parameter, dimension(2,2) :: B1=reshape((/.true._1,.true._1,.false._1,.true._1/),(/2,2/))
logical(2), parameter, dimension(2,2) :: B2=reshape((/.true._2,.true._2,.false._2,.true._2/),(/2,2/))
logical(4), parameter, dimension(2,2) :: B4=reshape((/.true._4,.true._4,.false._4,.true._4/),(/2,2/))
logical(8), parameter, dimension(2,2) :: B8=reshape((/.true._8,.true._8,.false._8,.true._8/),(/2,2/))

logical(1) :: B1res(2,2)=transpose(B1)
logical(2) :: B2res(2,2)=transpose(B2)
logical(4) :: B4res(2,2)=transpose(B4)
logical(8) :: B8res(2,2)=transpose(B8)

real(4), parameter, dimension(2,2) :: C4=reshape((/0._4,2._4,7._4,-5._4/),(/2,2/))
real(8), parameter, dimension(2,2) :: C8=reshape((/0._8,2._8,7._8,-5._8/),(/2,2/))
real(16), parameter, dimension(2,2) :: C16=reshape((/0._16,2._16,7._16,-5._16/),(/2,2/))

real(4) :: C4res(2,2)=transpose(C4)
real(8) :: C8res(2,2)=transpose(C8)
real(16) :: C16res(2,2)=transpose(C16)

complex(4), parameter, dimension(2,1) :: D4=reshape((/(0._4,2._4),(7._4,-5._4)/),(/2,1/))
complex(8), parameter, dimension(2,1) :: D8=reshape((/(0._8,2._8),(7._8,-5._8)/),(/2,1/))
complex(16), parameter, dimension(2,1) :: D16=reshape((/(0._16,2._16),(7._16,-5._16)/),(/2,1/))

complex(4) :: D4res(1,2)=transpose(D4)
complex(8) :: D8res(1,2)=transpose(D8)
complex(16) :: D16res(1,2)=transpose(D16)

character, parameter, dimension(3,1) :: E=reshape((/'a','b','c'/),(/3,1/))
character :: Eres(1,3)=transpose(E)

end
