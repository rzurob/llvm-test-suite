!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : typeParamInquiryIntrinsicAssociate05.f
!*
!*  DATE                       : August 8 2008
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
!* 4. ASSOCIATE WITH POINTER, ALLOCATABLE,POLYMORPHIC
!* 5. DEFECT 354781
!234567890123456789012345678901234567890123456789012345678901234567890

program typeParamInquiryIntrinsicAssociate05
    implicit none

    character(:),pointer :: p1
    character(:),pointer :: p2(:)
    class(*),pointer :: p3
    class(*),pointer :: p4(:)
    character(:),allocatable :: c1
    character(:),allocatable :: c2(:)

    character(len=7),target :: c3="xlftest"
    character(len=3),target:: c4(3)=['abc','efg','hij']


    !----- test 1-----!
    allocate(p1,source="xlftest")
    associate(x=>p1)
       if(x /="xlftest")                                    error stop 10_4
       if(x%len /= len(x) .or. x%len /= 7)                  error stop 11_4
    end associate

!     print *,p1(1:3),p1(1:3)%len,len(p1(1:3))
     !--- defect 354781--!
    associate(x=>p1(1:3))
       if(x /="xlf")                                        error stop 12_4
       if(x%len /= len(x) .or. x%len /= 3)                  error stop 13_4
    end associate

    print *,len(p1//" team")
    associate(x=>p1//" team")
       if(x /= "xlftest team")                               error stop 14_4
       if(x%len /= len(x) .or. x%len /= 12)                  error stop 15_4
    end associate

!     print *,p1(1:3)//" team meeting"(1:5),len(p1(1:3)//" team meeting"(1:5))
     !--- defect 354781--!
    associate(x=>p1(1:3)//" team meeting"(1:5))
       if(x /= "xlf team")                                  error stop 16_4
       if(x%len /= len(x) .or. x%len /= 8)                  error stop 17_4
    end associate

     !----- test 2-----!
     allocate(p2(3),source=["ab","cd","ef"])
     associate(x=>p2)
        if(any(x /= ["ab","cd","ef"]))                      error stop 18_4
        if(x%len /= 2)                                      error stop 19_4
     end associate

!     print *,p2(1:2)
     associate(x=>p2(1:2))
        if(any(x /= ["ab","cd"]))                           error stop 20_4
        if(x%len /= 2)                                      error stop 21_4
     end associate

!     print *,p2(1)(2:2),len(p2(1)(2:2))
     !---- defect 354781---!
     associate(x=>p2(1)(2:2))
        if(any(x /= ["b"]))                                  error stop 22_4
        if(x%len /= len(x) .or. x%len /= 1)                 error stop 23_4
     end associate

     associate(x=>p2//"123")
         if(any(x /=["ab123","cd123","ef123"]))              error stop 24_4
         if(x%len /= len(x) .or. x%len /= 5)                 error stop 25_4
     end associate

     associate(x=>p2//"123"(1:1))
         if(any(x /=["ab1","cd1","ef1"]))                    error stop 26_4
         if(x%len /= len(x) .or. x%len /= 3)                 error stop 27_4
     end associate

     associate(x=>p2(1:2)//"123"(1:1))
         if(any(x /=["ab1","cd1"]))                          error stop 28_4
         if(x%len /= len(x) .or. x%len /= 3)                 error stop 29_4
     end associate

!     print *,p2(1)(1:1)//"123"(1:1),len(p2(1)(1:1)//"123"(1:1))
     !---- defect 354781----!
     print *,p2(1)(1:1)//"123"(1:1)
     associate(x=>p2(1)(1:1)//"123"(1:1))
         if(any(x /=["a1"]))                                  error stop 30_4
         if(x%len /= len(x) .or. x%len /= 2)                 error stop 31_4
     end associate

     !---- test 3-----!
     allocate(c1,source="xlftest")
     associate(x=>c1)
       if(x /="xlftest")                                    error stop 32_4
       if(x%len /= len(x) .or. x%len /= 7)                  error stop 33_4
     end associate

     !--- defect 354781--!
!     print *,c1(1:3)
    associate(x=>c1(1:3))
      if(x /="xlf")                                        error stop 34_4
       if(x%len /= len(x) .or. x%len /= 3)                  error stop 35_4
    end associate

    associate(x=>c1//" team")
       if(x /= "xlftest team")                               error stop 36_4
       if(x%len /= len(x) .or. x%len /= 12)                  error stop 37_4
    end associate

     !--- defect 354781--!
!     print *,c1(1:3)//" team meeting"(1:5)
    associate(x=>c1(1:3)//" team meeting"(1:5))
       if(x /= "xlf team")                                  error stop 38_4
       if(x%len /= len(x) .or. x%len /= 8)                  error stop 39_4
    end associate


     !----- test 4-----!
     allocate(c2(3),source=["ab","cd","ef"])
     associate(x=>c2)
        if(any(x /= ["ab","cd","ef"]))                      error stop 40_4
        if(x%len /= 2)                                      error stop 41_4
     end associate

     associate(x=>c2(1:2))
        if(any(x /= ["ab","cd"]))                           error stop 42_4
        if(x%len /= 2)                                      error stop 43_4
     end associate

     !---- defect 354781---!
!      print *,c2(2:3)(2:2)
     associate(x=>c2(2)(2:2))
        if(any(x /= ['d']))                                 error stop 44_4
        if(x%len /= len(x) .or. x%len /= 1)                 error stop 45_4
     end associate

     associate(x=>c2//"123")
         if(any(x /=["ab123","cd123","ef123"]))              error stop 46_4
         if(x%len /= len(x) .or. x%len /= 5)                 error stop 47_4
     end associate

     associate(x=>c2//"123"(1:1))
         if(any(x /=["ab1","cd1","ef1"]))                    error stop 48_4
         if(x%len /= len(x) .or. x%len /= 3)                 error stop 49_4
     end associate

     associate(x=>c2(1:2)//"123"(1:1))
         if(any(x /=["ab1","cd1"]))                          error stop 50_4
         if(x%len /= len(x) .or. x%len /= 3)                 error stop 51_4
     end associate

     !---- defect 354781----!
     associate(x=>c2(1)(1:1)//"123"(1:1))
         if(any(x /=['a1']))                                 error stop 52_4
         if(x%len /= len(x) .or. x%len /= 2)                 error stop 53_4
     end associate

     !---- test 5------!
     p3=>c3
     associate(x=>p3)
       select type(x)
          type is(character(*))
!              print *,x
              if(x /= "xlftest")                              error stop 54_4
              if(x%len /= len(x) .or. x%len /= 7)             error stop 55_4
          class default
              error stop 100_4
       end select
     end associate

     select type(p3)
        type is(character(*))
           associate(x=>p3(1:3))
!              print *,x
             if(x /= "xlf")                                   error stop 56_4
             if(x%len /= len(x) .or. x%len /= 3)              error stop 57_4
           end associate
        class default
           error stop 101_4
     end select

     select type(p3)
        type is(character(*))
           associate(x=>p3(1:3)//" team")
!              print *,x
             if(x /= "xlf team")                              error stop 58_4
             if(x%len /= len(x) .or. x%len /= 8)              error stop 59_4
           end associate
        class default
           error stop 102_4
     end select

     !---- test 6-----!
     p4=>c4
     associate(x=>p4)
        select type(x)
           type is(character(*))
              if(any(x/= ['abc','efg','hij']))                error stop 60_4
              if(x%len /= len(x) .or. x%len /= 3)             error stop 61_4
           class default
              error stop 103_4
        end select
     end associate

     select type(p4)
        type is(character(*))
           associate(x=>p4(1:2))
             if(any(x /= ['abc','efg']))                      error stop 62_4
             if(x%len /= len(x) .or. x%len /= 3)              error stop 63_4
           end associate
        class default
          error stop 104_4
     end select

     select type(p4)
        type is(character(*))
           associate(x=>p4(1:2)(3:3))
             if(any(x /= ['c','g']))                          error stop 64_4
             if(x%len /= len(x) .or. x%len /= 1)              error stop 65_4
           end associate
        class default
          error stop 105_4
     end select

     select type(p4)
        type is(character(*))
           associate(x=>p4(1:2)(3:3)//p4(1:2)(1:1))
             if(any(x /= ['ca','ge']))                        error stop 66_4
             if(x%len /= len(x) .or. x%len /= 2)              error stop 67_4
           end associate
        class default
           error stop 106_4
     end select

end
