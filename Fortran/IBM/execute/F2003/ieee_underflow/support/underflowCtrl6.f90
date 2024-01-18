! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : underflowCtrl6.f
!*
!*  PROGRAMMER                 : Nancy Wang
!*  DATE                       : Nov. 15 2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : ieee_support_underflow_control(X)
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*  REFERENCE                  : Feature Number 289080
!*
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                :    
!*  test if ieee_support_underflow_control(X) returns false when passing 
!*  a user defined function with return type being a scalar of real type or 
!*  a pointer of real type , an allocatable of real type
!* 
!23456789012345678901234567890123456789012345678901234567890123456789012   
      module m
         use,intrinsic :: ieee_arithmetic
         contains

!          module procedure with return type being a real type scalar  
           real function modulePro1(x)
              complex,intent(in) :: x 
              modulePro1 = real(x)
           end function 

!          module procedure with return type being a pointer of real type scalar
           function modulePro2(rt)
              real,target,intent(in)  :: rt 
              real,pointer :: modulePro2
              modulePro2 => rt 
           end function   

!          module procedure with return type being an allocatable of real type scalar 
           function modulePro3()
              real,allocatable :: modulePro3 
              allocate(modulePro3)
              modulePro3=2.0
           end function 
 
      end module m     
 
      program underflowCtrl6
         use m
         implicit none
         interface
            function externalFun1(i,r)
               integer,intent(in) :: i 
               real,intent(in)    :: r 
               real               :: externalFun1 
            end function

            function externalFun2(p)
               real,pointer :: p,externalFun2 
            end function

            function externalFun3()
               real,allocatable :: externalFun3
            end function
         end interface

         real,target       :: a,b
         real,allocatable  :: ra

         a=1.0 ; b=2.0; 
         if(.not. allocated(ra))  allocate(ra)
   
         if(ieee_support_underflow_control(modulePro1((2.0,1.0)))) &
             error stop 101_4
         if(ieee_support_underflow_control(modulePro2(b)) )        &
             error stop 102_4
         if(ieee_support_underflow_control(modulePro3()) )         &
             error stop 103_4
         if(ieee_support_underflow_control(internalFun1()) )       &
             error stop 104_4
         if(ieee_support_underflow_control(internalFun2()) )       &
             error stop 105_4
         if(ieee_support_underflow_control(internalFun3()))        &
             error stop 106_4  
         if(ieee_support_underflow_control(externalFun1(2,a)) )    &
             error stop 107_4
         if(ieee_support_underflow_control(externalFun2(modulePro2(b)))) &
             error stop 108_4
         if(ieee_support_underflow_control(externalFun3()))        &
             error stop 109_4
          
         if(allocated(ra))  deallocate(ra)
         contains

!           internal function with return type being a real type scalar
            real function internalFun1()
               internalFun1 = a 
            end function

!           internal function with return type being a pointer of real type scalar
            function internalFun2()
               real,pointer :: internalFun2
               if( a > b ) then
                    internalFun2=>a
               else
                    internalFun2=>b
               endif
            end function

!           internal function with return type being allocatable of real type 
            function internalFun3()
               real,allocatable :: internalFun3
               allocate(internalFun3)
               internalFun3 = ra
            end function  
      end program

!     external function with return type being real type scalar 
      function externalFun1(i,r)
         implicit none
         integer,intent(in) :: i 
         real,intent(in)    :: r 
         real               :: externalFun1 
         externalFun1 = i * r   
      end function  

!     exteranl function with return type being pointer of real type scalar
      function externalFun2(p)
         implicit none
         real,pointer  :: p,externalFun2 
         externalFun2 =>p
      end function

!     external function with return type being allocatable of real type scalar
      function externalFun3()
         implicit none
         real,allocatable   :: externalFun3
         allocate(externalFun3)
         externalFun3 = 2.
      end function  
