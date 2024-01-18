! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2013-02-27
!*
!*  PRIMARY FUNCTIONS TESTED   : MASKL, MASKR intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MASKL(I[, KIND]), MASKR(I[, KIND])
!*
!*  Test the compilation fails if:
!*
!*  * first argument is not a nonnegative integer type less than or equal to the number of bits
!*  * second argument if exists, is not a scalar integer constant expression
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

program maskl_maskr

 real :: i1 = 2
 integer :: i2 = -2

 integer (kind = 1) :: j1 = 9
 integer (kind = 2) :: j2 = 17
 integer (kind = 4) :: j4 = 33
 integer (kind = 8) :: j8 = 65

 integer :: k1 = 1, k2 = 2, k4 = 4, k8 = 8

 integer , parameter :: k3 = 3, k5 = 5

!---- MASKL/R should not accept argument of any type other than integer ----!
print *, 'MASKL(i1)=       ', MASKL(i1)
print *, 'MASKL(2.0)=      ', MASKL(2.0)

print *, 'MASKR(i1)=       ', MASKR(i1)
print *, 'MASKR(2.0)=      ', MASKR(2.0)

!---- MASKL/R should not accept negative integer as first argument ---------!
print *, 'MASKL(-2)=       ', MASKL(-2)

print *, 'MASKR(-2)=       ', MASKR(-2)

!---- MASKL/R should not accept integer argument greater than the permitted bit size ----!
 !---- for KIND = 1
 print *, 'MASKL(9, 1)      ', MASKL(9,1)
 print *, 'MASKR(9,1)       ', MASKR(9,1)

 !---- for KIND = 2
 print *, 'MASKL(17,2)      ', MASKL(17,2)
 print *, 'MASKR(17,2)      ', MASKR(17,2)

 !---- for KIND = 4
 print *, 'MASKL(33,4)      ', MASKL(33,4)
 print *, 'MASKR(33,4)      ', MASKR(33,4)

 !---- for KIND = 8
 print *, 'MASKL(65,8)      ', MASKL(65,8)
 print *, 'MASKR(65,8)      ', MASKR(65,8)

!---- MAKL/R should not accept any second argument rather than scalar integer constant expression ----!

 print *, 'MASKL(2,k1)  ', MASKL(2,k1)
 print *, 'MASKL(2,k2)  ', MASKL(2,k2)
 print *, 'MASKL(2,k4)  ', MASKL(2,k4)
 print *, 'MASKL(2,k8)  ', MASKL(2,k8)

 print *, 'MASKR(2,k1)  ', MASKR(2,k1)
 print *, 'MASKR(2,k2)  ', MASKR(2,k2)
 print *, 'MASKR(2,k4)  ', MASKR(2,k4)
 print *, 'MASKR(2,k8)  ', MASKR(2,k8)

!---- MASKL/R should not accept invalid KIND arg values ----!

 print *, 'MASKL(5,k3)   ', MASKL(4,k3)
 print *, 'MASKR(5,k5)   ', MASKR(4,k5)

end

