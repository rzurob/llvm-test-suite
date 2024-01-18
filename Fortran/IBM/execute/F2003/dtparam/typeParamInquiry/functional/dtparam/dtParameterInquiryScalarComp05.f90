!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryScalarComp05.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : July 12 2008 
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
!* 3. DIFFERENT TYPE PARAMETER
!* 4. SCALAR LOGICAL COMPONENT
!* 5. DEFECT 353531
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   logical,parameter :: l_const1=.false. 
   logical,parameter :: l_const2=.true.
 
   type base(k1,k2,k3,k4,l1,l2,l3)
        integer(1),kind       :: k1
        integer(k1),kind      :: k2
        integer(k1%kind),kind :: k3
        integer(selected_int_kind(k1)),kind :: k4
        
        integer,len                      :: l1
        integer(kind=k1%kind),len        :: l2
        integer(int(4.2)),len            :: l3

        logical           :: l_1=.true.
        logical(k2)       :: l_2=.false.
        logical(k2%kind)  :: l_3=.true. .and. .false.
        logical(kind(k2)) :: l_4=.true. .or. .false.
        logical(kind(.true.))  :: l_5=l_const1
        logical(k1+k2)    :: l_6=l_const2
        logical(selected_int_kind(k1)) :: l_7= .not.(l_const1)
        logical(ichar(char(1)))        :: l_8=.not.(l_const2)
 
   end type
end module

  program dtParameterInquiryScalarComp05
  use m
  implicit none
  type(base(2,2,3,4,4,5,6)) :: t

  if(t%k1 /= 2 )                                              error stop 10_4
  if(t%k2 /= 2 )                                              error stop 11_4
  if(t%k3 /= 3 )                                              error stop 12_4
  if(t%k4 /= 4 )                                              error stop 13_4
  
  if(t%k1%kind /= kind(t%k1) .or. t%k1%kind /= 1)             error stop 14_4
  if(t%k2%kind /= kind(t%k2) .or. t%k2%kind /= 2)             error stop 15_4
  if(t%k3%kind /= kind(t%k3) .or. t%k3%kind /= 1)             error stop 16_4
  if(t%k4%kind /= kind(t%k4) .or. t%k4%kind /= 1)             error stop 17_4

  if(t%l1 /= 4)                                               error stop 18_4 
  if(t%l2 /= 5)                                               error stop 19_4
  if(t%l3 /= 6)                                               error stop 20_4  

  if(t%l1%kind /= kind(t%l1) .or. t%l1%kind /= 4)             error stop 21_4
  if(t%l2%kind /= kind(t%l2) .or. t%l2%kind /= 1)             error stop 22_4
  if(t%l3%kind /= kind(t%l3) .or. t%l3%kind /= 4)             error stop 23_4

  if(t%l_1 .neqv. .true.)                                     error stop 24_4
  if(t%l_2 .neqv. .false.)                                    error stop 25_4
  if(t%l_3 .neqv. .false.)                                    error stop 26_4
  if(t%l_4 .neqv. .true.)                                     error stop 27_4
  if(t%l_5 .neqv. .false.)                                    error stop 28_4
  if(t%l_6 .neqv. .true.)                                     error stop 29_4
  if(t%l_7 .neqv. .true.)                                     error stop 30_4
  if(t%l_8 .neqv. .false.)                                    error stop 31_4  

  if(t%l_1%kind /= kind(t%l_1) .or. t%l_1%kind /= 4)          error stop 32_4   
  if(t%l_2%kind /= kind(t%l_2) .or. t%l_2%kind /= 2)          error stop 33_4 
  !-- defect 353531--!
  if(t%l_3%kind /= kind(t%l_3) .or. t%l_3%kind /= 2)          error stop 34_4
  if(t%l_4%kind /= kind(t%l_4) .or. t%l_4%kind /= 4)          error stop 35_4
  if(t%l_5%kind /= kind(t%l_5) .or. t%l_5%kind /= 4)          error stop 36_4
  if(t%l_6%kind /= kind(t%l_6) .or. t%l_6%kind /= 4)          error stop 37_4
  if(t%l_7%kind /= kind(t%l_7) .or. t%l_7%kind /= 1)          error stop 38_4
  if(t%l_8%kind /= kind(t%l_8) .or. t%l_8%kind /= 1)          error stop 39_4

  end
