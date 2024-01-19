!*  ============================================================================
!*
!*  DATE                       : 2011-01-19
!*
!*  PRIMARY FUNCTIONS TESTED   : Complex Part Designator
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 383634
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  This program tests the complex part designator:
!*     Test the use in structure component, array element and array sections
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main

   implicit none

   type CPD
      complex :: c
   end type CPD

   integer i
   integer, parameter :: N = 4*2
   complex, dimension(N) :: c_a

   type(CPD) :: cpd1, cpd2(2,4)

   cpd1%c%RE = 3.12345678
   cpd1%c%IM = 2.12345678
   print *,"cpd1:",cpd1

   cpd2(:,:)%c = (0.0, 0.0)
   cpd2(:,:2)%c%RE = 5.5
   cpd2(:,:2)%c%IM = 3.5
   print *,"cpd2:",cpd2%c
   cpd2(:,:)%c = (0.0, 0.0)
   cpd2(:,2:)%c%RE = 5.5
   cpd2(:,2:)%c%IM = 3.5
   print *,"cpd2:",cpd2%c

   c_a = (0.0, 0.0)
   c_a(1:N/2)%RE = 101.5
   c_a(1:N/2)%IM = 100.5
   do i = 1, N/2
      if (c_a(i) .NE. (101.5, 100.5)) then
         print *,c_a(i)," .NE. ",(101.5, 100.5)
         ERROR STOP 201
      end if
   end do
   c_a(N/2+1:N)%RE = 201.5
   c_a(N/2+1:N)%IM = 200.5
   do i = N/2+1, N
      if (c_a(i) .NE. (201.5, 200.5)) then
         print *,c_a(i)," .NE. ",(201.5, 200.5)
         ERROR STOP 301
      end if
   end do

   c_a = (0.0, 0.0)
   c_a(1:N/4)%RE = 1.5
   c_a(1:N/4)%IM = 0.5
   do i = 1, N/4
      if (c_a(i) .NE. (1.5, 0.5)) then
         print *,c_a(i)," .NE. ",(1.5, 0.5)
         ERROR STOP 401
      end if
   end do
   c_a(N/4+1:N/2)%RE = 3.5
   c_a(N/4+1:N/2)%IM = 2.5
   do i = N/4+1, N/2
      if (c_a(i) .NE. (3.5, 2.5)) then
         print *,c_a(i)," .NE. ",(3.5, 2.5)
         ERROR STOP 501
      end if
   end do
   c_a(N/2+1:N-N/4)%RE = 5.5
   c_a(N/2+1:N-N/4)%IM = 4.5
   do i = N/2+1, N-N/4
      if (c_a(i) .NE. (5.5, 4.5)) then
         print *,c_a(i)," .NE. ",(5.5, 4.5)
         ERROR STOP 601
      end if
   end do
   c_a(N-N/4+1:N)%RE = 7.5
   c_a(N-N/4+1:N)%IM = 6.5
   do i = N-N/4+1, N
      if (c_a(i) .NE. (7.5, 6.5)) then
         print *,c_a(i)," .NE. ",(7.5, 6.5)
         ERROR STOP 701
      end if
   end do

   c_a = (0.0, 0.0)
   c_a(1:N/2)%RE = 101.5
   c_a(1:N/2)%IM = 100.5
   c_a(N/2+1:N)%RE = 201.5
   c_a(N/2+1:N)%IM = 200.5
   print *,"c_a:",c_a

end program main
