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
!* DESCRIPTION                : complex type
!* ===================================================================

implicit none

complex(4), parameter, dimension(2,2) :: C=reshape( &
 & (/(2.25e0,4.441e-6), (2.917e-5,4.2e-3), (1.521e-7,3.131e-5), (1.107e-1,1.7521e-7)/), &
 & (/2,2/))

complex(8), parameter, dimension(7,1) :: D=reshape( &
 & (/(6.6431327d-9,0.0_8), (0.0_8,3.9464967d-7), (6.1431828d-8,0.0_8), &
 &   (1.31872691d-5,0.0_8), (4.5153674d0,0.0_8), &
 &   (0.0_8,9.6935829d-3), (0.0_8,1.26118376d-9)/), &
 & (/7,1/))

complex(16), parameter, dimension(11,2) :: E=reshape( &
 & (/(31.2_16,-1.0_16), (71.2_16,-1.0_16), (43.2_16,-1.0_16), (22.2_16,-1.0_16), &
 &   (16.2_16,-1.0_16), (02.2_16,-1.0_16), (47.2_16,-1.0_16), (66.2_16,-1.0_16), &
 &   (75.2_16,-1.0_16), (17.2_16,-1.0_16), (88.2_16,-1.0_16), (65.2_16,-1.0_16), &
 &   (30.2_16,-1.0_16), (97.2_16,-1.0_16), (76.2_16,-1.0_16), (72.2_16,-1.0_16), &
 &   (06.2_16,-1.0_16), (21.2_16,-1.0_16), (10.2_16,-1.0_16), (41.2_16,-1.0_16), &
 &   (24.2_16,-1.0_16), (30.2_16,-1.0_16)/), &
 & (/11,2/))

complex(4), dimension(2,2) :: res3=transpose(C)
complex(8), dimension(1,7) :: res4=transpose(D)
complex(16), dimension(2,11) :: res5=transpose(E)

if (.not. all(res3 .eq. transpose(C))) stop 3
if (.not. all(res4 .eq. transpose(D))) stop 4
if (.not. all(res5 .eq. transpose(E))) stop 4

end
