!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : August 4 2008
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
!* 2. TYPE PARAMETER INQUIRY FOR INTRINSIC TYPE
!* 3. DEFERRED TYPE PARAMETER INQUIRY
!* 4. ALLOCATABLE AND POINTER ARRAY AS DUMMY ARGUMENT
!* 5. DEFECT 354606
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   character(:),allocatable :: a1(:)
   character(7),target      :: t1="xlf"//"test"//''
   character(:),pointer     :: p1(:)=>null()
   contains
      pure integer function getlen(c)
          character(*),intent(in)  ::c
           getlen=c%len+len(c)
      end function

      subroutine test4(p4)
         character(:),pointer,intent(in) :: p4(:)

         if(p4%len /= len(p4) .or. p4%len /= 12)           error stop 26_4
         if(ubound(p4,1) /= 14)                            error stop 27_4
         if(p4%kind /= kind(p4) .or. p4%kind /= 1)         error stop 28_4
         if(any(p4 /= "xlftest team"))                     error stop 29_4
      end subroutine

      subroutine test5(p5)
         character(:),pointer,intent(inout) :: p5(:)

         if(p5%len /= len(p5) .or. p5%len /= 12)           error stop 30_4
         if(ubound(p5,1) /= 14)                            error stop 31_4
         if(p5%kind /= kind(p5) .or. p5%kind /= 1)         error stop 32_4
         if(any(p5 /= "xlftest team"))                     error stop 33_4

         allocate( p5(getlen(t1(1:3))),source="xlftest"(1:3))

      end subroutine

      subroutine test6(p6)
         character(:),pointer,intent(out) :: p6(:)
         allocate(p6(t1%len:t1%kind+kind(t1)),source="test")
      end subroutine

end module
program typeParamInquiryIntrinsicDefer06
    use m
    implicit none

    character(3),parameter,dimension(3) :: a3=['abc','efg','hij']

    allocate(a1(len(a3)+ubound(a3,1)),source=a3(1)//'12345')
    call test1(a1)
    call test2(a1)
    if(a1%len /= len(a1) .or. a1%len /= 8)                error stop 18_4
    if(ubound(a1,1) /= 10)                                error stop 19_4
    if(a1%kind /= kind(a1) .or. a1%kind /= 1)             error stop 20_4
    if(any(a1 /= "xlf test"))                                  error stop 21_4

    call test3(a1)

    if((len(a1) /= 5) .or. (a1%len /= 5))                 error stop 22_4
    if(ubound(a1,1) /= 7)                                 error stop 23_4
    if(a1%kind /= kind(a1) .or. a1%kind /= 1)             error stop 24_4
    if(any(a1 /= "xlf t"))                                error stop 25_4

    allocate(p1(2*t1%len),source=t1//" team")
    call test4(p1)

    call test5(p1)
    if(p1%len /= len(p1) .or. p1%len /= 3)                error stop 34_4
    if(ubound(p1,1) /= 6)                                 error stop 35_4
    if(p1%kind /= kind(p1) .or. p1%kind /= 1)             error stop 36_4
    if(any(p1 /= "xlf"))                                  error stop 37_4

    call test6(p1)
    if(p1%len /= len(p1) .or. p1%len /= 4)                error stop 38_4
    if(ubound(p1,1) /= 0 .or. lbound(p1,1) /= 1)          error stop 39_4
    if(p1%kind /= kind(p1) .or. p1%kind /= 1)             error stop 40_4
    if(any(p1 /= ""))                                     error stop 41_4

    contains
      subroutine test1(p1)
         character(:),allocatable,intent(in) :: p1(:)
         if(p1%len /= len(p1) .or. p1%len /= 8)           error stop 10_4
         if(ubound(p1,1) /= 6)                            error stop 11_4
         if(p1%kind /= kind(p1) .or. p1%kind /= 1)        error stop 12_4
         if(any(p1 /= "abc12345"))                        error stop 13_4
      end subroutine

      subroutine test2(p2)
         character(:),allocatable,intent(inout) :: p2(:)

         if(p2%len /= len(p2) .or. p2%len /= 8)           error stop 14_4
         if(ubound(p2,1) /= 6)                            error stop 15_4
         if(p2%kind /= kind(p2) .or. p2%kind /= 1)        error stop 16_4
         if(any(p2 /= "abc12345"))                        error stop 17_4

         deallocate(p2)
         allocate(p2(len("xlftest"(1:3))+t1%len),source=t1(1:3))

         p2=t1(1:3)//" "//"xlftest"(4:)
      end subroutine

      subroutine test3(p3)
         character(:),allocatable,intent(out) :: p3(:)

         allocate(character(len=len(t1(1:2))+ len(t1(3:7)) + &
                   t1(1:2)%kind+kind(t1(3:7))) :: p3(t1%len) )

!          allocate(character(len=len(t1(1:2))+len(t1(3:7)) + &
!                   t1(1:2)%kind+kind(t1(3:7))) :: p3(t1%len) )

          p3=t1(1:3)//" "//"xlftest"(4:4)
      end subroutine

end


