!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : July 28 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : TYPE PARAMETER INQUIRY
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 6.1.3
!* 2. TYPE PARAMETER INQUIRY FOR INTRINSIC TYPE
!* 3. FUNCTION RESULT IS TYPE PARAMETER INQUIRY
!* 4. DEFECT 354413 354520
!234567890123456789012345678901234567890123456789012345678901234567890

program typeParamInquiryIntrinsicRes01
    implicit none

    integer(8),parameter :: i1=2
    integer(8) ::  i2=2

    character(len=*),parameter :: c1='abc'
    character(len=3) :: c2='abc'

    print *,"test 1"
    print *,getTP1(2_8),getTP2(2_8),getTP3(2_8)
    print *,getTP1(i1),getTP2(i1),getTP3(i1)
    print *,getTP1(i2),getTP2(i2),getTP3(i2)

    print *,"test 2"
    print *,getTP4('abc'),getTP5('abc'),getTP6('abc')
    print *,getTP4(c1),getTP5(c1),getTP6(c1)
    print *,getTP4(c2),getTP5(c2),getTP6(c2)

    print *,"test 3"
    print *,getTP7('abc'),getTP8('abc'),getTP9('abc')
    print *,getTP7(c1),getTP8(c1),getTP9(c1)
    print *,getTP7(c2),getTP8(c2),getTP9(c2)

    print *,"test 4"
    print *,getTP10('abc'),getTP11('abc','def'),getTP12('abc'//'efgh'//c1)
    print *,getTP10(c1),getTP11(c1,'def'),getTP12('123'//'    '//c1)
    print *,getTP10(c2),getTP11(c2,'###'),getTP12('!!!'//''//c1//'^^^^')
    contains

      function getTP1(a)
         integer(8) :: a
         integer(a%kind) getTP1
         getTP1=a%kind+kind(a)
      end function

      function getTP2(a)
         integer(8) :: a
         integer(kind(a)) getTP2
         getTP2=kind(a)+a%kind
      end function

      function getTP3(a)
         integer(8) :: a
         integer(a%kind) :: getTP3
         getTP3=kind(2_8) + a%kind
      end function

      function getTP4(c)
         character(*) :: c
         integer(c%kind) getTP4
         getTP4=c%len+len(c)
      end function

      function getTP5(c)
         character(*) :: c
         integer(kind(c)) getTP5
         getTP5=c%len+len(c)
      end function

      function getTP6(c)
         character(*) :: c
         integer(c%kind) :: getTP6
         getTP6=c%len+len(c)
      end function

      integer(kind(2_8)) function getTP7(c)
         character(*) :: c
         getTP7=c%kind+kind(c)
      end function

      integer(kind(2_4+2_8)) function getTP8(c)
         character(*) :: c
         getTP8=kind(c//'abc')+c%kind
      end function

      function getTP9(c)
         character(*) :: c
         integer :: getTP9
         getTP9=c%kind+kind(c)
      end function

      integer(2_4) function getTP10(c)
         character(*) :: c
         getTP10=len(c//'1234')+ c%len
      end function

      integer(2_4) function getTP11(arg1,arg2)
         character(*) :: arg1,arg2
         getTP11=arg1%len + len(arg2) + len('1234')
      end function

      integer(max(2,4)) function getTP12(arg)
         character(*) :: arg
         getTP12=arg%len
      end function

end

