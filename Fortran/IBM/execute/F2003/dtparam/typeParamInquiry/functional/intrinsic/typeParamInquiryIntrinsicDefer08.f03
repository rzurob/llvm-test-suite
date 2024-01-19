!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : August 4 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : TYPE PARAMETER INQUIRY
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 6.1.3
!* 2. TYPE PARAMETER INQUIRY FOR INTRINSIC TYPE
!* 3. DEFERRED TYPE PARAMETER INQUIRY
!* 4. ALLOCATABLE AND POINTER
!* 5. ACTUAL ARGUMENT PASS THROUGH MULTIPLE SUBROUTINE
!* 6. DEFECT 354520 354606
!234567890123456789012345678901234567890123456789012345678901234567890

module m

    contains

      subroutine test1(a2)
           character(:),allocatable,intent(inout)  :: a2(:)

           call test2(a2)
           contains
              subroutine test2(a3)
                    character(:),allocatable,intent(inout)  :: a3(:)

                    call test3(a3)
              end subroutine
      end  subroutine
     subroutine  test3(a4)
         character(:),allocatable,intent(inout)  :: a4(:)

           if(a4%len /= len(a4) .or. a4%len /= 4)           error stop 10_4
           if(a4%kind /=kind(a4) .or. a4%kind /= 1)         error stop 11_4
           if(ubound(a4,1) /= 5)                            error stop 12_4
           if(any(a4 /= "this"))                            error stop 13_4

           if(allocated(a4)) deallocate(a4)
           allocate(a4(3),source=["abc","def","ghi"])

     end subroutine
end module

program typeParamInquiryIntrinsicDefer08
    use m
    implicit none
    interface
    subroutine test6(b4)
        character(*),intent(inout) :: b4
    end subroutine
    subroutine test7(c1)
        character(:),pointer,intent(inout) :: c1(:)
    end subroutine
    end interface
    character(:), allocatable  :: a(:)
    character(:),allocatable   :: b
    character(:),pointer       :: c(:)
    character(len=*),parameter :: d(3)=(/"this","is  ","test"/)
    integer ::  i=0

    allocate(a(d(2:)%len+1),source=d(1))

    call test1(a)

    if(a%len /= len(a) .or. a%len /= 3)                 error stop 14_4
    if(a%kind /=kind(a) .or. a%kind /= 1)               error stop 15_4
    if(ubound(a,1) /=3)                                 error stop 16_4
    if(any(a /= ["abc","def","ghi"]) )                  error stop 17_4

    allocate(b, source="xlftest"(4:)//"team")
    call test4(b)

    if(b%len /= len(b) .or. b%len /= 8)                 error stop 21_4
    if(b%kind /= kind(b) .or. b%kind /= 1)              error stop 22_4
    if(b /= "hello")                                    error stop 23_4

    allocate(character(ubound(a,1) + ubound(d,1)+ len(d(3)(1:2))) :: &
              c(ubound(a,1) : ubound(d,1)+ len(d(3)(1:2))) )


    c=(/(char(i),i=1,ubound(c,1))/)
    call test7(c)

    if(c%len /= len(c) .or. c%len /= 4)                 error stop 28_4
    if(c%kind /= kind(c) .or. c%kind /= 1)              error stop 29_4
    if(any(c /= ["abcd","1234"]))                       error stop 30_4
    if(ubound(c,1) /= 2)                                error stop 31_4

    contains
       subroutine test4(b2)
          class(*),intent(out)  :: b2

          select type(b2)
             type is (character(*))
                 call test5(b2)
             class default
               error stop 100_4
          end select
       end subroutine

       subroutine test5(b3)
          character(*),intent(inout)  :: b3

             call test6(b3)
       end subroutine
end

subroutine test6(b4)
   character(*),intent(inout)  :: b4

   if(b4%len /= len(b4) .or. b4%len /= 8)              error stop 18_4
   if(b4%kind /= kind(b4) .or. b4%kind /= 1)           error stop 19_4
   if(b4 /= "testteam")                                error stop 20_4

   b4="hello world"(1:5)

end subroutine

subroutine test7(c1)
   character(:),pointer,intent(inout) :: c1(:)
   call test8(c1)
   contains
     subroutine test8(c2)
        character(:),pointer,intent(inout) :: c2(:)
        call test9(c2)
     end subroutine
     subroutine test9(c3)
        character(:),pointer,intent(inout) :: c3(:)

        if(c3%len /=len(c3) .or. c3%len /=8)            error stop 24_4
        if(c3%kind /= kind(c3) .or. c3%kind /= 1)       error stop 25_4
        if(any(c3 /= (/(char(i),i=1,ubound(c3,1))/) ))  error stop 26_4
        if(lbound(c3,1) /= 3 .or. ubound(c3,1) /= 5)    error stop 27_4

        deallocate(c3)
        allocate(c3(2),source=["abcd","1234"])

     end subroutine

end subroutine

