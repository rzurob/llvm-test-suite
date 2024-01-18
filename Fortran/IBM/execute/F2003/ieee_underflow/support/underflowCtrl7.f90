! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : underflowCtrl7.f
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
!*  a user defined function with return type being real type array or 
!*  a pointer of real type array, an allocatable of real type array
!* 
!23456789012345678901234567890123456789012345678901234567890123456789012   
      module m
         use,intrinsic :: ieee_arithmetic
         contains

!          module procedure with return type being a real type array  
           function modulePro1(arr)
              real,intent(in)   :: arr(:)
              real,dimension(size(arr)) :: modulePro1
              modulePro1 = 2*arr   
           end function 

!          module procedure with return type being a pointer of real type array 
           function modulePro2(n)
              real,pointer  :: modulePro2(:)
              allocate(modulePro2(n))
              modulePro2 = (/(i,i=1,n)/)             
           end function   

!          module procedure with return type being an allocatable of real type array 
           function modulePro3(n)
               integer,intent(in)  :: n
               real,allocatable,dimension(:,:) :: modulePro3 
               allocate(modulePro3(n,n))
               modulePro3=reshape((/(i,i=1,n*n)/),(/n,n/))
           end function    
      end module m     
 
      program underflowCtrl7
         use m
         implicit none
         interface
            function externalFun1(r1,r2)
               real,dimension(:),intent(in) :: r1,r2
               real,dimension(size(r1))     :: externalFun1
            end function

            function externalFun2(rp)
               real,pointer,intent(in)    :: rp(:)
               real,pointer,dimension(:)  :: externalFun2
            end function

            function externalFun3(ra)
               real,allocatable,intent(in)  :: ra(:)
               real,allocatable   :: externalFun3(:)
            end function
         end interface
          
         integer             :: i
         integer,parameter   :: n=3
         real,dimension(n)   :: r1=(/(i*i,i=1,n)/) 
         real,dimension(n)   :: r2=(/(-i*i,i=1,n)/)
         real,target,dimension(n) :: rt=(/(i,i=1,n)/)
         real,pointer,dimension(:) :: rp1=>null(),rp2=>null()
         real,allocatable    :: ra(:)

         rp1=>rt
         if(.not. allocated(ra))  allocate(ra(n))
         ra=r2
         rp2=>modulePro2(n) 

         if(ieee_support_underflow_control(modulePro1(r1)))              &
             error stop 101_4
         if(ieee_support_underflow_control(rp2))                         &
             error stop 102_4
         if(ieee_support_underflow_control(modulePro3(n)))               &
             error stop 103_4
         if(ieee_support_underflow_control(internalFun1(r1,r2)))         &
             error stop 104_4
         if(ieee_support_underflow_control(internalFun2(rp2)))           &
             error stop 105_4
         if(ieee_support_underflow_control(internalFun3()))              &
             error stop 106_4
         if(ieee_support_underflow_control(externalFun1(r1,r2)))         &
             error stop 107_4
         if(ieee_support_underflow_control(externalFun2(rp1)))           &
             error stop 108_4
         if(ieee_support_underflow_control(externalFun3(ra)))            &
             error stop 109_4

         if(allocated(ra))    deallocate(ra)
         deallocate(rp2)
         nullify(rp1,rp2)
 
         contains

!           internal function with return type being real type array 
            function internalFun1(r1,r2)
               real,intent(in),dimension(:) ::r1,r2
               real,dimension(size(r1)) :: internalFun1
               internalFun1=r1+r2
            end function 

!           internal function with return type being pointer of real type array
            function internalFun2(p)
               real,pointer,intent(in)  :: p(:)
               real,pointer    :: internalFun2(:)
               internalFun2 => p 
            end function 

!           internal function with return type being allocatable of real type array
            function internalFun3()
               real,allocatable   :: internalFun3(:,:)
               allocate(internalFun3(size(ra),size(ra)))
               internalFun3 = 1.0 
            end function
      end program

!    external function with return type being real type array 
     function externalFun1(r1,r2)
        real,dimension(:),intent(in) :: r1,r2
        real,dimension(size(r1))     :: externalFun1
        do i = 1 , size(r1) 
           if(r1(i) .ge. r2(i)) then
               externalFun1(i)=r1(i)
           else 
               externalFun1(i)=r2(i)
           endif
        enddo 
     end function 

!    exteranl function with return type being a pointer of real type array
     function externalFun2(rp)
        real,pointer,intent(in) :: rp(:)
        real,pointer,dimension(:)  :: externalFun2
        externalFun2 =>rp
     end function

!    external function with return type being an allocatable of real type array
     function externalFun3(ra)
        real,allocatable,intent(in)  :: ra(:)
        real,allocatable   :: externalFun3(:)
        allocate(externalFun3(size(ra)))
        externalFun3=ra
     end function  
