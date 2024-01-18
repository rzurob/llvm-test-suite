!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : CONJG intrinsic
!*
!* DESCRIPTION                : langlvl message
!* ===================================================================

complex(4), parameter :: c4=(12.34, 56.78)
complex(8), parameter :: c8=(1.234d4, -5.678D2)
complex(16), parameter :: c16=(-12.34q-34, -56.78q3)

complex(4) :: c4a=conjg(c4)
complex(8) :: c8a=conjg(c8)
complex(16) :: c16a=conjg(c16)

end
