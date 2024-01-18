!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : complexPartDesignatorF6.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Shahid Alam
!*  DATE                       : 2011-01-12
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Complex Part Designator
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 383634
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  This program tests the complex part designator:
!*     Set the value (single and double precision) and verify it
!*     using different trignometric functions sin, cos and tan
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main

   implicit none

   external precision_r4; logical(4) precision_r4
   external precision_r8; logical(4) precision_r8
   external precision_r16; logical(4) precision_r16
   integer i
   integer, parameter :: N = 300
   real(4), dimension(N) :: RS
   real(8), dimension(N) :: RD               ! Double precision real
   real(16), dimension(N) :: RDD
   complex(4), dimension(N)  :: CS
   complex(8), dimension(N)  :: CD           ! Double precision complex
   complex(16), dimension(N)  :: CDD

   ! Initialize the single and double precision real arrays
   do i = 1, N
      RS(i)  = 1.12345 * i
      RD(i)  = 1.123456789 * i
      RDD(i) = 1.12345678943215678 * i
   end do

   ! Initialize and test the complex array with single precision
   do i = 1, N
      CS(i) = (RS(i), RS(i)/2)
   end do
   do i = 1, N
      if (.NOT. precision_r4 (sin(CS(i)%RE), sin(RS(i))) ) then
         print *,sin(CS(i)%RE)," .NE. ",sin(RS(i))
         ERROR STOP 1
      else if (.NOT. precision_r4 (sin(CS(i)%IM), sin(RS(i)/2))) then
         print *,sin(CS(i)%IM)," .NE. ",sin(RS(i)/2)
         ERROR STOP 2
      end if
      if (.NOT. precision_r4 (cos(CS(i)%RE), cos(RS(i)))) then
         print *,sin(CS(i)%RE)," .NE. ",cos(RS(i))
         ERROR STOP 3
      else if (.NOT. precision_r4 (cos(CS(i)%IM), cos(RS(i)/2))) then
         print *,cos(CS(i)%IM)," .NE. ",cos(RS(i)/2)
         ERROR STOP 4
      end if
      if (.NOT. precision_r4 (tan(CS(i)%RE), tan(RS(i)))) then
         print *,sin(CS(i)%RE)," .NE. ",sin(RS(i))
         ERROR STOP 5
      else if (.NOT. precision_r4 (tan(CS(i)%IM), tan(RS(i)/2))) then
         print *,tan(CS(i)%IM)," .NE. ",tan(RS(i)/2)
         ERROR STOP 6
      end if
   end do

   ! Initialize and test the complex array with double precision
   do i = 1, N
      CD(i) = (RD(i), RD(i)/2)
   end do
   do i = 1, N
      if (.NOT. precision_r8 (sin(CD(i)%RE), sin(RD(i)))) then
         print *,sin(CD(i)%RE)," .NE. ",sin(RD(i))
         ERROR STOP 11
      else if (.NOT. precision_r8 (sin(CD(i)%IM), sin(RD(i)/2))) then
         print *,sin(CD(i)%IM)," .NE. ",sin(RD(i)/2)
         ERROR STOP 12
      end if
      if (.NOT. precision_r8 (cos(CD(i)%RE), cos(RD(i)))) then
         print *,cos(CD(i)%RE)," .NE. ",cos(RD(i))
         ERROR STOP 13
      else if (.NOT. precision_r8 (cos(CD(i)%IM), cos(RD(i)/2))) then
         print *,cos(CD(i)%IM)," .NE. ",cos(RD(i)/2)
         ERROR STOP 14
      end if
      if (.NOT. precision_r8 (tan(CD(i)%RE), tan(RD(i)))) then
         print *,tan(CD(i)%RE)," .NE. ",tan(RD(i))
         ERROR STOP 15
      else if (.NOT. precision_r8 (tan(CD(i)%IM), tan(RD(i)/2))) then
         print *,tan(CD(i)%IM)," .NE. ",tan(RD(i)/2)
         ERROR STOP 16
      end if
   end do

   ! Initialize and test the complex array with double double precision
   do i = 1, N
      CDD(i) = (RDD(i), RDD(i)/2)
   end do
   do i = 1, N
      if (.NOT. precision_r16 (sin(CDD(i)%RE), sin(RDD(i)))) then
         print *,sin(CDD(i)%RE)," .NE. ",sin(RDD(i))
         ERROR STOP 17
      else if (.NOT. precision_r16 (sin(CDD(i)%IM), sin(RDD(i)/2))) then
         print *,sin(CDD(i)%IM)," .NE. ",sin(RDD(i)/2)
         ERROR STOP 18
      end if
      if (.NOT. precision_r16 (cos(CDD(i)%RE), cos(RDD(i)))) then
         print *,cos(CDD(i)%RE)," .NE. ",cos(RDD(i))
         ERROR STOP 19
      else if (.NOT. precision_r16 (cos(CDD(i)%IM), cos(RDD(i)/2))) then
         print *,cos(CDD(i)%IM)," .NE. ",cos(RDD(i)/2)
         ERROR STOP 20
      end if
      if (.NOT. precision_r16 (tan(CDD(i)%RE), tan(RDD(i)))) then
         print *,tan(CDD(i)%RE)," .NE. ",tan(RDD(i))
         ERROR STOP 21
      else if (.NOT. precision_r16 (tan(CDD(i)%IM), tan(RDD(i)/2))) then
         print *,tan(CDD(i)%IM)," .NE. ",tan(RDD(i)/2)
         ERROR STOP 22
      end if
   end do

end program main
