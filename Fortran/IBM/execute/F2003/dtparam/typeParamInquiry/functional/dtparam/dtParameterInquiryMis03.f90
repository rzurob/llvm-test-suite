!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryMis03.f   
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
!*  DRIVER STANZA              :
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90 
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 6.1.3 
!* 2. TYPE PARAMETER INQUIRY
!* 3. COMPLICATED EXPRESSION AS LENGTH TYPE PARAMETER  
!* 4. DEFECT 354585,354602
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l)
     integer,len  :: l
   end type
end module

  program dtParameterInquiryMis03 
  use m
  implicit none

  character(len=*),parameter :: c1="hello"
  integer(8) :: i1=kind(4)
  type(base(3)) :: b1=base(3)()
  integer(2),dimension(3:6) :: i2
  type(base(:)),pointer     :: p1=>null()
  type(base(:)),allocatable :: b2

  !--- defect 354585--!
  allocate(base(l=i1+i1%kind + &
                c1%len+len(c1) + &
                getlen(b1)) :: p1)

  if(p1%l /= 25)                                      error stop 10_4
  allocate(base(i1*2 + lbound(i2,1) + ubound(i2,1) + &
               i2%kind + kind(i2) + max(4,int(2_8)) + &
               b1%l + b1%l%kind + kind(b1%l) + &
               getlen(b1) + kind(c1) ) :: b2)

  print *,i1*2,lbound(i2,1),ubound(i2,1)
  print *,i2%kind,kind(i2),max(4,int(2_8))
  print *,b1%l,b1%l%kind,kind(b1%l)
  print *,getlen(b1),kind(c1)

  print *, i1*2 + lbound(i2,1) + ubound(i2,1) + &
           i2%kind + kind(i2)  + max(4,int(2_8)) + &
           b1%l + b1%l%kind + kind(b1%l) + &
           getlen(b1) + kind(c1) 

  if(b2%l /= 40)                                      error stop 11_4

  deallocate(b2)
  !--- defect 354602---!
  allocate(base(kind(4)) :: b2)

  if(b2%l /= 4)                                       error stop 12_4

  contains
    integer function getlen(dt)
       type(base(*)) :: dt
       getlen=dt%l
    end function
end
