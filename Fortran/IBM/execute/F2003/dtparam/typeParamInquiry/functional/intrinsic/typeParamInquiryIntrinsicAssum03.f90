!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : August 10 2008
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
!* 3. DUMMY ARGUMENT HAS ASSUMED LENGTH
!* 4. TEST FUNCTION RESULT
!* 5. DEFECT 354846
!234567890123456789012345678901234567890123456789012345678901234567890
module m

     character(len=*),parameter :: c1="xlftest"
     character(len=*),parameter :: c3(3)=["abc","def","ghi"]
end module
program typeParamInquiryIntrinsicAssum03
    use m
    implicit none

    character(:),allocatable :: c2
    character(:),pointer :: c4(:) =>null()
    character(:),pointer :: c5=>null()

    allocate(c2,source=getchar1(c1))
    if(c2%len /= len(c2) .or. c2%len /= 14)            error stop 10_4
    if(c2 /= "xlftestxlftest")                         error stop 11_4

    deallocate(c2)
    allocate(c2,source=getchar1(c1(1:3)))
    if(c2%len /= len(c2) .or. c2%len /= 6)             error stop 12_4
    if(c2 /= "xlfxlf")                                 error stop 13_4


    allocate( c4(len(getchar2(c3)) ),source=getchar2(c3))
    if(c4%len /= len(c4) .or. c4%len /= 6)             error stop 14_4
    if(size(c4) /= 6)                                  error stop 15_4
    if(any(c4 /= ['a1','d2','g3','c4','f5','i6']))     error stop 16_4

    deallocate(c2)
    allocate(c2,source=getchar3(c1(1:3),c1(4:)))
    if(c2%len /= len(c2) .or. c2%len /= 7)             error stop 17_4
    if(c2 /= "xlftest")                                error stop 18_4

    deallocate(c4)
    c4=>getchar4(c3,['123','456','789'])
    if(c4%len /= len(c4) .or. c4%len /= 6)             error stop 19_4
    if(any(c4 /= ['abc123','def456','ghi789']))        error stop 20_4

    c4=>getchar4(c3(:)(1:2),c3(:)(3:3)//['12','34','56'])
    if(c4%len /= len(c4) .or. c4%len /= 5)             error stop 21_4
    if(any(c4 /= ['abc12','def34','ghi56']))           error stop 22_4

    c5=>getchar5(c1(1:2),len(c1(1:2)))
    if(c5%len /= len(c5) .or. c5%len /= 4)             error stop 23_4
    if(c5 /= "xlxl")                                   error stop 24_4

     c4=>getchar6(c3(1:2),len(c3(1:2)))
     if(c4%len /= len(c4) .or. c4%len /= 6)             error stop 25_4
     if(size(c4) /= 2)                                  error stop 26_4
     if(any(c4 /= ['abcabc','defdef']))                 error stop 27_4

     c4=>getchar6(c3(1:2)(1:2),len(c3(1:2)(1:2)))
     print *,c4%len,len(c4),c4,size(c4)
     if(c4%len /= len(c4) .or. c4%len /= 4)             error stop 28_4
     if(size(c4) /= 2)                                  error stop 29_4
     if(any(c4 /= ['abab','dede']))                     error stop 30_4

    contains

    function getchar1(ch)
       character(*),intent(in) :: ch
       character(ch%len+len(ch)) :: getchar1
       getchar1=ch//ch
    end function

    function getchar2(ch)
       character(*),intent(in) :: ch(:)
       character(ch%len+len(ch)) :: getchar2(2*size(ch))
       getchar2(1:size(ch))=ch(1:3)(1:1)//['1','2','3']
       getchar2(size(ch)+1:2*size(ch))=ch(1:3)(3:3) //['4','5','6']
    end function

    function getchar3(ch1,ch2)
       character(*),intent(in) :: ch1,ch2
       character(:),pointer :: getchar3
       allocate(getchar3,source=ch1//ch2)

    end function

    function getchar4(ch1,ch2)
       character(*),intent(in) :: ch1(:),ch2(:)
       character(:),pointer :: getchar4(:)
       allocate(getchar4(size(ch1)),source=ch1//ch2)
    end function

    function getchar5(ch,len)
       character(*),intent(in) :: ch
       integer :: len
       character(:),pointer :: getchar5
       allocate(character(ch%len+len) :: getchar5)
       getchar5=ch//ch
    end function

    function getchar6(ch,len)
       character(*),intent(in) :: ch(:)
       integer :: len
       character(:),pointer :: getchar6(:)
       allocate(character(ch%len+len) :: getchar6(size(ch)))
       getchar6=ch//ch
    end function

end

