!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : August 7 2008
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
!* 3. TYPE PARAMETER INQUIRY INSIDE ASSOCIATE
!* 4. SELECTOR IS CHARACTER EXPRESSION
!* 5. 3 ICES IN THIS TEST CASE, DEFECT 354758,354736,354781
!234567890123456789012345678901234567890123456789012345678901234567890

program typeParamInquiryIntrinsicAssociate03
    implicit none

    character(len=*),parameter :: c1="good"
    character(len=10) :: c2="morning"
    character(len=3) :: c3(3)=['abc','def','ghi']
    character(:),allocatable :: c4
    character(:),allocatable :: c5(:)
    integer :: i

    associate(x=>c1//c2(1:4)//"ing")
      if(x%len /= len(x) .or. x%len /= 11)            error stop 10_4
      if(x /= "goodmorning")                          error stop 11_4
    end associate

    associate(x=>c3(1:2)(1:1))
      if(x%len /= len(x) .or. x%len /= 1)             error stop 12_4
      if(any(x /= ['a','d']))                         error stop 13_4
    end associate

!    print *,c3(1:2)(1:1)//'1',len(c3(1:2)(1:1)//'1'),size(c3(1:2)(1:1)//'1')
    associate(x=>c3(1:2)(1:1)//'1')      !<== defect 354758
      if(x%len /= len(x) .or. x%len /= 2)             error stop 14_4
      if(any(x /= ['a1','d1']))                       error stop 15_4
       if(size(x) /= 2)                               error stop 16_4
    end associate

!    print *,c3(1)//'1',len(c3(1)//'1')
    associate(x=>c3(1)//'1')             !<== defect 354736
      if(x%len /= len(x) .or. x%len /= 4)             error stop 17_4
      if(x /= 'abc1')                                 error stop 18_4
    end associate

    associate(x=>c1(1:1)//c2(1:1)//'abc'(1:1))
       if(x%len /= len(x) .or. x%len /= 3)             error stop 19_4
       if(x /= 'gma')                                  error stop 20_4
    end associate

    call setchar1(c4,c1(1:2),c2(5:7))
    associate(x=>c4)
        if(x /= "going home")                          error stop 21_4
        if(x%len /= len(x) .or. x%len /= 10)           error stop 22_4
    end associate

    if(allocated(c4))  deallocate(c4)

    call setchar2(c5,c3(1:2),c3(2:3))

    associate(x=>c5)
       if(x%len /= len(x) .or. x%len /= 10)            error stop 23_4
       if(size(x) /= 2)                                error stop 24_4
       if(any(x /= ['abcdef end','defghi end']))       error stop 25_4
    end associate

!     print *,c5(1)(1:1),len(c5(1)(1:1)),c5(1)(1:1)
    associate(x=>c5(1)(1:1))            !<==defect 354781
        if(x%len /= len(x) .or. x%len /= 1)           error stop 26_4
         if(x /= 'a')                                 error stop 27_4
    end associate

    if(allocated(c5))   deallocate(c5)
    call setchar3(c5,c3(1:2)(1:1),c3(2:3)(3:3))
    associate(x=>c5)
       if(x%len /= len(x) .or. x%len /= 6)             error stop 28_4
       if(size(x) /= 2)                                error stop 29_4
       if(any(x /= ['af end','di end']))               error stop 30_4
    end associate

    contains

       subroutine setchar1(c,c1,c2)
          character(*),intent(in) :: c1,c2
          character(:),allocatable,intent(out) :: c

          associate(x1=>c1//c2)
              if(x1%len /=len(x1) .or. x1%len /= 5)    error stop 31_4
              if(x1 /= "going")                        error stop 32_4
              allocate(c,source=x1//' home')
          end associate
       end subroutine

       subroutine setchar2(c,c1,c2)
          character(*),intent(in) :: c1(:),c2(:)
          character(:),allocatable,intent(out) :: c(:)

          associate(x1=>c1//c2)
              if(x1%len /=len(x1) .or. x1%len /= 6)    error stop 33_4
              if(any(x1 /= ['abcdef','defghi']))       error stop 34_4
              allocate(c(size(c1)),source=x1//' end')
          end associate
       end subroutine

       subroutine setchar3(c,c1,c2)
          character(*),intent(in) :: c1(:),c2(:)
          character(:),allocatable,intent(out) :: c(:)

          associate(x1=>c1//c2)
              if(x1%len /=len(x1) .or. x1%len /= 2)     error stop 35_4
              if(any(x1 /= ['af','di']))                error stop 36_4
              allocate(c(size(c1)),source=x1//' end')
          end associate
       end subroutine
end

