!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryArrayCharComp01.f
!*
!*  DATE                       : July 17 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : TYPE PARAMETER INQUIRY
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 6.1.3
!* 2. TYPE PARAMETER INQUIRY FOR DT AND COMPONENT
!* 3. DIFFERENT TYPE PARAMETER
!* 4. CHARACTER ARRAY COMPONENT
!* 5. DEFECT 353810 353821 353566 353331 353615 355124
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   type base(k1,k2,l1,l2)
      integer(1),kind :: k1=2
      integer(k1),kind :: k2=2

      integer(k1),len :: l1=k1
      integer(k2),len :: l2=k2

      character(4) :: c1(3)
      character(len=k1) :: c2(k1)
      character(len=k1+k2),dimension(2,3) :: c3(1,2)
      character(kind=1,len=k1) :: c4(1,1)
      character(len=k2):: c5(k1,k2)
      character(len=k1%kind) :: c6(kind(k1))
      character(k1%kind+k2%kind) :: c7(k1) ! defect 353566
      character(4) :: c8(k1%kind+k2%kind)  ! defect 353566
      character(kind(k1+k2)) :: c9(kind(k1+k2)) ! defect 353821
      character(kind(k1)+kind(k2)) :: c10(kind(k1)+kind(k2)) ! defect 353615
      character(len=l1+l2) :: c11(l1+l2)
      character(l1)    :: c12(l2)
      character(kind(l1)) :: c13(kind(l1)) ! defect 355124
      character(l2%kind)  :: c14(l2%kind)
      character(kind(l1+l2))       :: c15(kind(l1+l2)) ! defect 355124
      character(kind(l1)+kind(l2)) :: c16(kind(l1)+kind(l2)) ! 355124
      character(l1%kind+l2%kind) :: c17(l1%kind+l2%kind)   !defect 353566
      character(l1+k1),dimension(l1:l1+k1) :: c18 ! defect 353331
      character(kind(l2+k2)),dimension(kind(l2+k2)) :: c19 ! defect 355124
      character(kind(l2)+k2) :: c20(kind(l2)+k2) ! defect 355124
      character(l2%kind+k2%kind) :: c21(l2%kind+k2%kind) ! defect 353566
      character(max(l1,l2)) :: c22(l1:l2) ! defect 353810
      character(4*k1) :: c23(k1,k2)

   end type

end module

  program dtParameterInquiryArrayCharComp01
  use m
  implicit none

  type(base)  :: t

  if(t%k1 /= 2)                                         error stop 10_4
  if(t%k2 /= 2)                                         error stop 11_4
  if(t%l1 /= 2)                                         error stop 12_4
  if(t%l2 /= 2)                                         error stop 13_4

  if(t%k1%kind /= kind(t%k1) .or. t%k1%kind /= 1)       error stop 14_4
  if(t%k2%kind /= kind(t%k2) .or. t%k2%kind /= 2)       error stop 15_4
  if(t%l1%kind /= kind(t%l1) .or. t%l1%kind /= 2)       error stop 16_4
  if(t%l2%kind /= kind(t%l2) .or. t%l2%kind /= 2)       error stop 17_4

  if(t%c1%kind /= kind(t%c1) .or. t%c1%kind /= 1)       error stop 18_4
  if(t%c2%kind /= kind(t%c2) .or. t%c2%kind /= 1)       error stop 19_4
  if(t%c3%kind /= kind(t%c3) .or. t%c3%kind /= 1)       error stop 20_4
  if(t%c4%kind /= kind(t%c4) .or. t%c4%kind /= 1)       error stop 21_4
  if(t%c5%kind /= kind(t%c5) .or. t%c5%kind /= 1)       error stop 22_4
  if(t%c6%kind /= kind(t%c6) .or. t%c6%kind /= 1)       error stop 23_4

  if(t%c7%kind /= kind(t%c7) .or. t%c7%kind /= 1)       error stop 24_4
  if(t%c8%kind /= kind(t%c8) .or. t%c8%kind /= 1)       error stop 25_4
  if(t%c9%kind /= kind(t%c9) .or. t%c9%kind /= 1)       error stop 26_4
  if(t%c10%kind /= kind(t%c10) .or. t%c10%kind /= 1)    error stop 27_4
  if(t%c11%kind /= kind(t%c11) .or. t%c11%kind /= 1)    error stop 28_4
  if(t%c12%kind /= kind(t%c12) .or. t%c12%kind /= 1)    error stop 29_4

  if(t%c13%kind /= kind(t%c13) .or. t%c13%kind /= 1)    error stop 30_4
  if(t%c14%kind /= kind(t%c14) .or. t%c14%kind /= 1)    error stop 31_4
  if(t%c15%kind /= kind(t%c15) .or. t%c15%kind /= 1)    error stop 32_4
  if(t%c16%kind /= kind(t%c16) .or. t%c16%kind /= 1)    error stop 33_4
  if(t%c17%kind /= kind(t%c17) .or. t%c17%kind /= 1)    error stop 34_4
  if(t%c18%kind /= kind(t%c18) .or. t%c18%kind /= 1)    error stop 35_4

  if(t%c19%kind /= kind(t%c19) .or. t%c19%kind /= 1)    error stop 36_4
  if(t%c20%kind /= kind(t%c20) .or. t%c20%kind /= 1)    error stop 37_4
  if(t%c22%kind /= kind(t%c22) .or. t%c22%kind /= 1)    error stop 38_4
  if(t%c23%kind /= kind(t%c23) .or. t%c23%kind /= 1)    error stop 39_4
  if(t%c21%kind /= kind(t%c21) .or. t%c21%kind /= 1)    error stop 40_4

  if(t%c1%len  /= len(t%c1)  .or. t%c1%len /= 4)        error stop 42_4
  if(t%c2%len  /= len(t%c2)  .or. t%c2%len /= 2)        error stop 43_4
  if(t%c3%len  /= len(t%c3)  .or. t%c3%len /= 4)        error stop 44_4
  if(t%c4%len  /= len(t%c4)  .or. t%c4%len /= 2)        error stop 45_4
  if(t%c5%len  /= len(t%c5)  .or. t%c5%len /= 2)        error stop 46_4
  if(t%c6%len  /= len(t%c6)  .or. t%c6%len /= 1)        error stop 47_4
  if(t%c7%len  /= len(t%c7)  .or. t%c7%len /= 3)        error stop 48_4
  if(t%c8%len  /= len(t%c8)  .or. t%c8%len /= 4)        error stop 49_4
  if(t%c9%len  /= len(t%c9)  .or. t%c9%len /= 2)        error stop 50_4
  if(t%c10%len  /= len(t%c10)  .or. t%c10%len /= 3)     error stop 51_4
  if(t%c11%len  /= len(t%c11)  .or. t%c11%len /= 4)     error stop 52_4
  if(t%c12%len  /= len(t%c12)  .or. t%c12%len /= 2)     error stop 53_4
  if(t%c13%len  /= len(t%c13)  .or. t%c13%len /= 2)     error stop 54_4
  if(t%c14%len  /= len(t%c14)  .or. t%c14%len /= 2)     error stop 55_4
  if(t%c15%len  /= len(t%c15)  .or. t%c15%len /= 2)     error stop 56_4
  if(t%c16%len  /= len(t%c16)  .or. t%c16%len /= 4)     error stop 57_4
  if(t%c17%len  /= len(t%c17)  .or. t%c17%len /= 4)     error stop 58_4
  if(t%c18%len  /= len(t%c18)  .or. t%c18%len /= 4)     error stop 59_4
  if(t%c19%len  /= len(t%c19)  .or. t%c19%len /= 2)     error stop 60_4
  if(t%c20%len  /= len(t%c20)  .or. t%c20%len /= 4)     error stop 61_4
  if(t%c21%len  /= len(t%c21)  .or. t%c21%len /= 4)     error stop 62_4
  if(t%c22%len  /= len(t%c22)  .or. t%c22%len /= 2)     error stop 63_4
  if(t%c23%len  /= len(t%c23)  .or. t%c23%len /= 8)     error stop 64_4

  end
