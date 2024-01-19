!*********************************************************************
!*  ===================================================================
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
!* 4. ASSOCIATE WITH FUNCTION RESULT
!* 5. DEFECT 354606
!234567890123456789012345678901234567890123456789012345678901234567890

program typeParamInquiryIntrinsicAssociate04
    implicit none

    integer(2) :: intval=5
    character(len=7) :: c1="xlftest"
    character(len=*),parameter :: c2(3)=['abcd','efgh','ijkl']

    associate(x=>getvalue(intval))
       if(x /= 7)                                           error stop 10_4
       if(x%kind /= kind(x) .or. x%kind /= 8)               error stop 11_4
    end associate

    associate(x=>getchar1(c1))
       if(x /= "xlftest")                                   error stop 12_4
       if(x%len /= len(x) .or. x%len /= 7)                  error stop 13_4

       if(len(x(1:3)) /= 3)   error stop 14_4
       if(x(1:3) /= "xlf")                                  error stop 15_4
    end associate

    associate(x=>getchar1(c1(1:3)))
       if(x /= "xlf")                                       error stop 16_4
       if(x%len /= len(x) .or. x%len /= 3)                  error stop 17_4
    end associate

    associate(x=>getchar1(c1(1:3))//'test')
       if(x /= "xlftest")                                   error stop 18_4
       if(x%len /= len(x) .or. x%len /= 7)                  error stop 19_4
    end associate

    associate(x=>getchar2(c1(1:3),c1(4:)))
       if(x /= "xlftest")                                   error stop 20_4
       if(x%len /= len(x) .or. x%len /= 7)                  error stop 21_4
       associate(x1=>x(1:2)//"f")
          if(x1 /= "xlf")                                   error stop 22_4
          if(x1%len /= len(x1) .or. x1%len /= 3)            error stop 23_4
       end associate
    end associate

    associate(x=>getchar2(c1(1:3),c1(4:))//" team")
       if(x /= "xlftest team")                              error stop 24_4
       if(x%len /= len(x) .or. x%len /= 12)                 error stop 25_4
    end associate

    associate(x=>getchar3(c1(1:3),c1(4:)))
       if(x /= "xt")                                        error stop 26_4
       if(x%len /= len(x) .or. x%len /= 7)                  error stop 27_4
       associate(x1=>x(1:2)//"est"(1:1))
          if(x1 /= "xte")                                   error stop 28_4
          if(x1%len /= len(x1) .or. x1%len /= 3)            error stop 29_4
       end associate
    end associate

    associate(x=>getchar4(c2(1:2),c2(3:3)))
       if(any(x /= ['ab','ef','kl']))                       error stop 30_4
       if(x%len /= len(x) .or. x%len /= 8)                  error stop 31_4
       associate(x1=>x(2:2)(1:1))
          if(any(x1 /= 'e'))                                error stop 32_4
          if(x1%len /= len(x1) .or. x1%len /= 1)            error stop 33_4
       end associate
    end associate

   associate(x=>getlen(c1))
       if(x /= 14)                                          error stop 34_4
       if(x%kind /= kind(x) .or. x%kind /= 2)               error stop 35_4
   end associate

   associate(x=>c1%len)
       if(x /= 7)                                           error stop 36_4
   end associate

    contains

      integer(8) function getvalue(intvalue)
          integer(2),intent(in) :: intvalue
          getvalue=intvalue%kind + intvalue
      end function

      integer(2) function getlen(ch)
          character(*),intent(in) :: ch
          getlen=2*ch%len
      end function

      function getchar1(ch)
         character(*),intent(in) :: ch
         character(ch%len) :: getchar1
         getchar1=ch
      end function

      function getchar2(ch1,ch2)
         character(*),intent(in) :: ch1,ch2
         character(ch1%len+ch2%len) :: getchar2
         getchar2=ch1//ch2
      end function

      function getchar3(ch1,ch2)
         character(*),intent(in) :: ch1,ch2
         character(ch1%len+ch2%len) :: getchar3
         getchar3=ch1(1:1)//ch2(1:1)
      end function

      function getchar4(ch1,ch2)
         character(*),intent(in) :: ch1(:),ch2(:)
         character(:),allocatable :: getchar4(:)
         allocate(character(ch1%len+ch2%len) :: &
                    getchar4(size(ch1)+size(ch2)))
         getchar4(1:size(ch1)) = ch1(:)(1:2)
         getchar4(size(ch1)+1 : size(ch1)+size(ch2)) = ch2(:)(3:4)
      end function
end
