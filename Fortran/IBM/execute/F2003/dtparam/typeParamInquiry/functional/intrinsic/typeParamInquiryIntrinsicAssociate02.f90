!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : typeParamInquiryIntrinsicAssociate02.f
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
!* 4. SELECTOR IS SCALAR VARIABLE
!234567890123456789012345678901234567890123456789012345678901234567890
program typeParamInquiryIntrinsicAssociate02
    implicit none

    integer(kind('a')) :: i1
    integer(max(1,2))  :: i2
    integer(selected_int_kind(7)) :: i3
    integer(kind(2_4)+kind(2_4))  :: i4
    integer(kind=2),parameter     :: i5=2_4

    real(4*kind('a'))  :: r1
    real(4*i5)         :: r2
    real(8*i5%kind)    :: r3
    real(4),parameter  :: r4=4.0_8

    complex(int(4.0))                  :: x1
    complex(selected_real_kind(10,20)) :: x2
    complex(16)                        :: x3
    complex(kind(1_8)),parameter         :: x4=(1.e2,2.e2)

    logical(1)              :: l1
    logical(kind(i5))       :: l2
    logical(min(kind(4),8)) :: l3
    logical(max(l1%kind,8)) :: l4
    logical(selected_int_kind(2)),parameter :: l5=.false.

    character(len=*),parameter :: c1="good"
    character(kind=1,len=3)    :: c2="he"
    character(len=7)           :: c3="team"
    character(len('abc'))      :: c4="abc"
    character(len('abcdefg'(1:4))) :: c5="abcdefg"(1:4)
    character(c1%len)              :: c6=c1
    character(c1(1:2)%len)         :: c7=c1(1:2)
    character(len(c1)+len('abc'))  :: c8=c1//'abc'
    character(len('abc'//'efg'(:2)))   :: c9='abc'//'efg'
    character(c1(1:1)%len+len(c1(4:4))) :: c10=c1(1:1)//c1(4:4)

    !-------------verify integer-----------------!
    associate(i_1=>i1)
       if(i_1%kind /= kind(i_1) .or. i_1%kind /= 1)           error stop 10_4
    end associate

    associate(i_2=>i2)
       if(i_2%kind /= kind(i_2) .or. i_2%kind /= 2)           error stop 11_4
    end associate

    associate(i_3=>i3)
       if(i_3%kind /= kind(i_3) .or. i_3%kind /= 4)           error stop 12_4
    end associate

    associate(i_4=>i4)
       if(i_4%kind /= kind(i_4) .or. i_4%kind /= 8)           error stop 13_4
    end associate

    associate(i_5=>i5)
       if(i_5%kind /= kind(i_5) .or. i_5%kind /= 2)           error stop 14_4
    end associate

    !-------------verify real---------------------!
    associate(r_1=>r1)
       if(r_1%kind /= kind(r_1) .or. r_1%kind /= 4)           error stop 15_4
    end associate

    associate(r_2=>r2)
       if(r_2%kind /= kind(r_2) .or. r_2%kind /= 8)           error stop 16_4
    end associate

    associate(r_3=>r3)
       if(r_3%kind /= kind(r_3) .or. r_3%kind /= 16)          error stop 17_4
    end associate

    associate(r_4=>r4)
       if(r_4%kind /= kind(r_4) .or. r_4%kind /= 4)           error stop 18_4
    end associate

    !-------------verify complex------------------!
    associate(x_1=>x1)
       if(x_1%kind /= kind(x_1) .or. x_1%kind /= 4)           error stop 19_4
    end associate

    associate(x_2=>x2)
       if(x_2%kind /= kind(x_2) .or. x_2%kind /= 8)           error stop 20_4
    end associate

    associate(x_3=>x3)
       if(x_3%kind /= kind(x_3) .or. x_3%kind /= 16)          error stop 21_4
    end associate

    associate(x_4=>x4)
       if(x_4%kind /= kind(x_4) .or. x_4%kind /= 8)           error stop 22_4
    end associate

    !-------------verify logical------------------!
    associate(l_1=>l1)
       if(l_1%kind /= kind(l_1) .or. l_1%kind /= 1)           error stop 23_4
    end associate

    associate(l_2=>l2)
       if(l_2%kind /= kind(l_2) .or. l_2%kind /= 2)           error stop 24_4
    end associate

    associate(l_3=>l3)
       if(l_3%kind /= kind(l_3) .or. l_3%kind /= 4)           error stop 25_4
    end associate

    associate(l_4=>l4)
       if(l_4%kind /= kind(l_4) .or. l_4%kind /= 8)           error stop 26_4
    end associate

    associate(l_5=>l5)
       if(l_5%kind /= kind(l_5) .or. l_5%kind /= 1)           error stop 27_4
    end associate

    !-------------verify character------------------!
    associate(c_1=>c1)
       if(c_1%kind /= kind(c_1) .or. c_1%kind /= 1)           error stop 28_4
       if(c_1%len /= len(c_1) .or. c_1%len /= 4)              error stop 29_4
       if(c_1 /= 'good')                                      error stop 30_4
    end associate

    associate(c_2=>c2)
       if(c_2%kind /= kind(c_2) .or. c_2%kind /= 1)           error stop 31_4
       if(c_2%len /= len(c_2) .or. c_2%len /= 3)              error stop 32_4
       if(c2 /= 'he')                                         error stop 33_4
    end associate

    associate(c_3=>c3)
       if(c_3%kind /= kind(c_3) .or. c_3%kind /= 1)           error stop 34_4
       if(c_3%len /= len(c_3) .or. c_3%len /= 7)              error stop 35_4
       if(c_3 /= 'team')                                      error stop 36_4
    end associate

    associate(c_4=>c4)
       if(c_4%kind /= kind(c_4) .or. c_4%kind /= 1)           error stop 55_4
       if(c_4%len /= len(c_4) .or. c_4%len /= 3)              error stop 56_4
       if(c_4 /= 'abc')                                       error stop 57_4
    end associate

    associate(c_5=>c5)
       if(c_5%kind /= kind(c_5) .or. c_5%kind /= 1)           error stop 37_4
       if(c_5%len /= len(c_5) .or. c_5%len /= 4)              error stop 38_4
       if(c_5 /= 'abcd')                                      error stop 39_4
    end associate

    associate(c_6=>c6)
       if(c_6%kind /= kind(c_6) .or. c_6%kind /= 1)           error stop 40_4
       if(c_6%len /= len(c_6) .or. c_6%len /= 4)              error stop 41_4
       if(c_6 /= 'good')                                      error stop 42_4
    end associate

    associate(c_7=>c7)
       if(c_7%kind /= kind(c_7) .or. c_7%kind /= 1)           error stop 43_4
       if(c_7%len /= len(c_7) .or. c_7%len /= 2)              error stop 44_4
       if(c_7 /= 'go')                                        error stop 45_4
    end associate

    associate(c_8=>c8)
       if(c_8%kind /= kind(c_8) .or. c_8%kind /= 1)           error stop 46_4
       if(c_8%len /= len(c_8) .or. c_8%len /= 7)              error stop 47_4
       if(c_8 /= 'goodabc')                                   error stop 48_4
    end associate

    associate(c_9=>c9)
       if(c_9%kind /= kind(c_9) .or. c_9%kind /= 1)           error stop 49_4
       if(c_9%len /= len(c_9) .or. c_9%len /= 5)              error stop 50_4
       if(c_9 /= 'abcef')                                     error stop 51_4
    end associate

    associate(c_10=>c10)
       if(c_10%kind /= kind(c_10) .or. c_10%kind /= 1)        error stop 52_4
       if(c_10%len /= len(c_10) .or. c_10%len /= 2)           error stop 53_4
       if(c_10 /= 'gd')                                       error stop 54_4
    end associate

end

