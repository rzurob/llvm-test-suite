!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : typeParamInquiryIntrinsicDefer05.f
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
!* 4. ALLOCATABLE AND POINTER SCALAR AS DUMMY ARGUMENT
!* 5. DEFECT 354606
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   character(:),allocatable :: a1
   character(7),target :: t1="xlf"//"test"//''
   character(:),pointer     :: p1=>null()
   character(:),pointer     :: p2=>null()
   contains
      pure integer function getlen(c)
          character(*),intent(in)  ::c
           getlen=c%len+len(c)
      end function

      subroutine test4(pp1)
         character(:),pointer,intent(in) :: pp1

         if(pp1 /= "xlftest" )                            error stop 25_4
         if(pp1%len /= len(pp1) .or. pp1%len /= 7)        error stop 26_4
         if(pp1%kind /= kind(pp1) .or. pp1%kind /=1)      error stop 27_4

      end subroutine

      subroutine test5(pp2)
         character(:),pointer,intent(inout) :: pp2

         if(pp2%len /= len(pp2) .or. pp2%len /= 12)       error stop 28_4
         if(pp2%kind /= kind(pp2) .or. pp2%kind /=1)      error stop 29_4

         allocate(pp2,source=t1(4:))

      end subroutine

      subroutine test6(pp2)
         character(:),pointer,intent(out) :: pp2
         allocate(character(getlen(t1(1:3)//t1(4:7))) :: pp2)
         pp2=t1(1:3)//t1(4:7)
      end subroutine

end module
program typeParamInquiryIntrinsicDefer05
    use m
    implicit none

    character(:),allocatable :: a2
    character(*),parameter :: a3="efg"

    allocate(a1,source="xlftest")
    a2=a1//' '//"team"
    call test1(a1)
    call test2(a2)
    if(a2 /= "test")                                     error stop 16_4
    if(a2%len /= len(a2) .or. a2%len /= 4)               error stop 17_4
    if(a2%kind /= kind(a2) .or. a2%kind /=1)             error stop 18_4

    call test3(a2)

    if(a2%len /=len(a2) .or. a2%len /= 12)               error stop 20_4
    if(a2%kind /= kind(a2) .or. a2%kind /=1)             error stop 21_4

    p1=>t1
    if(p1%len /= len(p1) .or. p1%len /= 7)               error stop 22_4
    if(p1%kind /= kind(p1) .or. p1%kind /=1)             error stop 23_4
    if(p1 /= "xlftest" )                                 error stop 24_4

    call test4(p1)
    allocate(character(getlen('abc')+len('abc')+len(t1(1:3))) :: p1)
    p2=>p1
    call test5(p2)
    if(p2 /= "test")                                      error stop 30_4
    if(p2%len /= len(p2) .or. p2%len /= 4)                error stop 31_4
    if(p2%kind /= kind(p2) .or. p2%kind /=1)              error stop 32_4

    call test6(p2)

    if(p2 /="xlftest")                                    error stop 33_4
    if(p2%len /= len(p2) .or. p2%len /= 14)               error stop 34_4
    if(p2%kind /= kind(p2) .or. p2%kind /=1)              error stop 35_4

    contains
      subroutine test1(aa1)
         character(:),allocatable,intent(in) :: aa1

         if(aa1 /= "xlftest" )                            error stop 10_4
         if(aa1%len /= len(aa1) .or. aa1%len /= 7)        error stop 11_4
         if(aa1%kind /= kind(aa1) .or. aa1%kind /=1)      error stop 12_4
      end subroutine

      subroutine test2(aa2)
         character(:),allocatable,intent(inout) :: aa2
         if(aa2 /= "xlftest team" )                       error stop 13_4
         if(aa2%len /= len(aa2) .or. aa2%len /= 12)       error stop 14_4
         if(aa2%kind /= kind(aa2) .or. aa2%kind /=1)      error stop 15_4

         if(allocated(aa2))  deallocate(aa2)
            allocate(aa2,source=a1(4:))

      end subroutine

      subroutine test3(aa2)
         character(:),allocatable,intent(out) :: aa2

         if(allocated(aa2))                               error stop 19_4
         allocate(character(len=getlen('abc')+getlen(a3)) :: aa2)
      end subroutine

end


