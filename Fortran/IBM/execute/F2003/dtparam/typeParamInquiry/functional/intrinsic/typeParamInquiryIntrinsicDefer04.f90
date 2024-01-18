!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : typeParamInquiryIntrinsicDefer04.f
!*
!*  DATE                       : August 2 2008
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
!* 4. ALLOCATABLE AND POINTER ARRAY
!* 5. USE ALLOCATE
!* 6. DEFECT 354583
!234567890123456789012345678901234567890123456789012345678901234567890

program typeParamInquiryIntrinsicDefer04
    implicit none

    character(len=5) :: c1="xlf"
    character(len=*),parameter :: c2="xlftest"

    character(:),allocatable :: a1(:)
    character(:),allocatable :: a2(:)
    character(:),pointer  :: p1(:)

    allocate(a1(c1%len),source=['a','b','c','d','e'])
    if(a1%len /= len(a1) .or. a1%len /=1 .or. len(a1) /=1)    error stop 10_4
    if(ubound(a1,1) /= 5)                                     error stop 11_4
    deallocate(a1)

    allocate(character(len(c1)) :: a1(c2%len))
    a1="Hi"
    if(a1%len /= len(a1) .or. a1%len /=2 .or. len(a1) /=2)    error stop 12_4
    if(ubound(a1,1) /= 7)                                     error stop 13_4

    allocate(a2(a1%len:ubound(a1,1)),source=a1(1))
    if(a2%len /= len(a2) .or. a2%len /=2 .or. len(a2) /=2)    error stop 14_4
    if(lbound(a2,1) /=2 .or. ubound(a2,1) /= 7)               error stop 15_4
    deallocate(a1,a2)

    allocate(a2(c1%len:c2%len),source='test')
    allocate(a1(len(c1)+2:c2%len+2),source=a2)
    if(a1%len /= len(a1) .or. a1%len /=4 .or. len(a1) /=4)    error stop 16_4
    if(a2%len /= len(a2) .or. a2%len /=4 .or. len(a2) /=4)    error stop 17_4
    if(lbound(a1,1) /= 7 .or. ubound(a1,1) /= 9)              error stop 18_4
    if(lbound(a2,1) /= 5 .or. ubound(a2,1) /= 7)              error stop 19_4
    deallocate(a1,a2)

    allocate(character(-5) :: a1(a1%kind:kind(a1)+a1%kind))
    if(a1%len /= len(a1) .or. a1%len /=0 .or. len(a1) /=0)    error stop 20_4
    if(lbound(a1,1) /= 1 .or. ubound(a1,1) /= 2)              error stop 21_4

    allocate(a2(3:2),source='')
    if(a2%len /= len(a2) .or. a2%len /=0 .or. len(a2) /=0)    error stop 22_4
    if(lbound(a2,1) /= 1 .or. ubound(a2,1) /= 0)              error stop 23_4


    allocate(p1(c1%len:getlen(c1)),source=c1)
    if(p1%len /= len(p1) .or. p1%len /= 5)                    error stop 24_4
    if(lbound(p1,1) /= 5 .or. ubound(p1,1) /= 10)             error stop 25_4

    deallocate(p1)
    associate(x=> getlen('abc'//'efg')+kind('abc') + &
                  max(1,2) + ubound(a1,1) + a1%kind + &
                  len(a2) + c2%len )
       allocate(character(x) :: p1(x))
       if(p1%len /= 25 )                                      error stop 26_4
       if(ubound(p1,1) /= 25)                                 error stop 27_4
    end associate

    if(allocated(a1))  deallocate(a1)
    if(allocated(a2))  deallocate(a2)
    deallocate(p1)
    contains
       integer function getlen(a)
          character(*),intent(in) :: a
          getlen=2*a%len
       end function
end

