!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryResParam01.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : July 26 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : TYPE PARAMETER INQUIRY
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 6.1.3 
!* 2. TYPE PARAMETER INQUIRY
!* 3. FUNCTION RESULT IS INTEGER TYPE PARAMETER
!* 4. TYPE PARAMETER HAS DEFAULT INITIALIZATION 
!* 5. DEFECT 354417
!234567890123456789012345678901234567890123456789012345678901234567890
module m
     type base(k1,k2,l1,l2)
        integer(int(4.0)+kind(4_4)),kind :: k1=2
        integer(max(2,kind(3.0))),kind    :: k2=4
        integer(selected_int_kind(2)),len :: l1=2
        integer(k1),len :: l2= 3
     end type
     contains
       integer function getTotalParam1(dt)
          type(base(2,4,*,*)),intent(in) :: dt
          
          getTotalParam1=dt%k1+dt%k2+dt%l1+dt%l2
       end function
  
       integer function getTotalParam2(k1,k2,l1,l2)
           real :: k1,k2,l1,l2

          getTotalParam2=int(k1)+int(k2)+int(l1)+int(l2)
       end function
end module

  program dtParameterInquiryResParam01 
  use m
  implicit none
 
  interface
    function getKindParamKind1(dt) result (res)
        import 
        type(base(2,4,*,*)),intent(in) :: dt
        integer(dt%k1%kind) res
     end function

  function getKindParamKind2(dt)
      import
      type(base(2,4,*,*)),intent(in) :: dt
      integer(dt%k2%kind) :: getKindParamKind2
  end function

  function getKind3(dt)
      import
      type(base(2,4,*,*)),intent(in) :: dt
      integer(dt%k1) getKind3

  end function

  function getKind4(dt)
      import 
      type(base(2,4,*,*)),intent(in) :: dt
      integer(dt%k2) :: getKind4

  end function

  integer(8) function getKind5(k1)
      import
      integer(8) :: k1
  end function

  function getKind6(k2)
      import
      integer(k2%kind) :: k2  
      integer(k2%kind) :: getKind6

  end function

  function getLenKind1(dt)
      import 
      type(base(2,4,*,*)),intent(in) :: dt
      integer(kind(dt%l1)) :: getLenKind1
  end function

  function getLenKind2(dt)
      import 
      type(base(2,4,*,*)),intent(in) :: dt
      integer(dt%l2%kind) :: getLenKind2
  end function

  end interface 
  
  interface getTotalParam
      module procedure getTotalParam1,getTotalParam2
  end interface


  type(base) :: b1

  if(getTotalParam(b1) /= 11)                             error stop 10_4
  if(getTotalParam(real(b1%k1),real(b1%k2),  &
             real(b1%l1),real(b1%l2)) /= 11)              error stop 11_4

  if(getkind1() /= 2)                                     error stop 12_4
  if(getkind2() /= 4)                                     error stop 13_4
  if(getlen1()  /= 2)                                     error stop 14_4
  if(getlen2()  /= 3)                                     error stop 15_4
  if(getKindParamKind1(b1) /= 8)                          error stop 16_4
  if(getKindParamKind2(b1) /= 4)                          error stop 17_4
  if(getKind3(b1) /= 2)                                   error stop 18_4
  if(getKind4(b1) /= 4)                                   error stop 19_4
  if(getKind5(b1%k1) /= 2)                                error stop 20_4
  if(getKind6(b1%k2) /= 4)                                error stop 21_4
  if(getLenKind1(b1) /= 1)                                error stop 22_4
  if(getLenKind2(b1) /= 2)                                error stop 23_4

  contains
     integer(b1%k1%kind) function getkind1()
        getkind1=b1%k1
     end function    
     integer(kind(b1%k2)) function getkind2()
        getkind2=b1%k2
     end function
     integer(kind(b1%l1)) function getlen1()
        getlen1=b1%l1
     end function
     integer(kind(b1%l2)) function getlen2()
        getlen2=b1%l2
     end function

end

  function getKindParamKind1(dt) result(res)
      use m
      type(base(2,4,*,*)),intent(in) :: dt 
      integer(dt%k1%kind) res
      res=dt%k1%kind
  end function

  function getKindParamKind2(dt)
      use m
      type(base(2,4,*,*)),intent(in) :: dt
      integer(dt%k2%kind) :: getKindParamKind2
      getKindParamKind2=dt%k2%kind
  end function

  function getKind3(dt)
      use m
      type(base(2,4,*,*)),intent(in) :: dt
      integer(dt%k1) getKind3

      getKind3=dt%k1
  end function

  function getKind4(dt)
      use m
      type(base(2,4,*,*)),intent(in) :: dt
      integer(dt%k2) :: getKind4

      getKind4=dt%k2
  end function

  integer(8) function getKind5(k1)
      use m 
      integer(8) :: k1
      getKind5=k1
  end function

  function getKind6(k2)
      use m 
      integer(k2%kind) :: k2
      integer(k2%kind) :: getKind6
      getKind6=k2
  end function

  function getLenKind1(dt)
      use m
      type(base(2,4,*,*)),intent(in) :: dt
      integer(kind(dt%l1)) :: getLenKind1
      getLenKind1=dt%l1%kind
  end function

  function getLenKind2(dt)
      use m
      type(base(2,4,*,*)),intent(in) :: dt
      integer(dt%l2%kind) :: getLenKind2
      getLenKind2=dt%l2%kind

  end function
