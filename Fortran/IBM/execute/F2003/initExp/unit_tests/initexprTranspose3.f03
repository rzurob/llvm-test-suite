!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : TRANSPOSE intrinsic
!*
!* DESCRIPTION                : real type
!* ===================================================================

implicit none

real(4), parameter, dimension(2,2) :: C=reshape( &
 & (/2.2504441e6, 2.917542e3, 1.52173131e5, 1.10717521e7/), &
 & (/2,2/))

real(8), parameter, dimension(7,1) :: D=reshape( &
 & (/6.6431327d-9, 3.9464967d-7, 6.1431828d-8, 1.31872691d-5, 4.5153674d0, &
 &   9.6935829d-3, 1.26118376d-9/), &
 & (/7,1/))

real(16), parameter, dimension(11,2) :: E=reshape( &
 & (/31.2_16, 71.2_16, 43.2_16, 22.2_16, 16.2_16, 02.2_16, 47.2_16, 66.2_16, 75.2_16, 17.2_16, &
 &   88.2_16, 65.2_16, 30.2_16, 97.2_16, 76.2_16, 72.2_16, 06.2_16, 21.2_16, 10.2_16, 41.2_16, &
 &   24.2_16, 30.2_16/), &
 & (/11,2/))

real(4), dimension(2,2) :: res3=transpose(C)
real(8), dimension(1,7) :: res4=transpose(D)
real(16), dimension(2,11) :: res5=transpose(E)

if (.not. all(res3 .eq. transpose(C))) error stop 3
if (.not. all(res4 .eq. transpose(D))) error stop 4
if (.not. all(res5 .eq. transpose(E))) error stop 4

end
