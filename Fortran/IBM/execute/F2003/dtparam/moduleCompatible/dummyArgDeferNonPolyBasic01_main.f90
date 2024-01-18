!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArgDeferNonPolyBasic01.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Nov. 6 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DUMMY ARGUMENT WITH DEFERRED LENGTH 
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*  1. DERIVED TYPE HAS NO COMPONENT
!*  2. DUMMY ARGUMENT IS SCALAR OR ARRAY
!*  3. USE MODULE SUBROUTINE, EXTERNAL SUBROUTINE,INTERNAL SUBROUTINE 
!*  4. VERIFY ALLOCATION AND ASSOCIATION STATUS AND TYPE PARAMETER VALUE
!234567890123456789012345678901234567890123456789012345678901234567890

program dummyArgDeferNonPolyBasic01
  use m
  implicit none

  interface

  subroutine extsub1(arg)
     import

     type(dtp(2,4,l1=:,l2=:)),allocatable :: arg(:)

  end subroutine

  subroutine extsub2(arg)
     import

     type(dtp(2,4,:,:)),pointer       :: arg

  end subroutine

  end interface  
  type(dtp(2,4,:,:)),allocatable :: dtp1
  type(dtp(2,4,:,:)),pointer     :: dtp2(:)=>null()   
  type(dtp(2,4,:,:)),allocatable :: dtp3(:)
  type(dtp(2,4,:,:)),pointer     :: dtp4=>null()

 
  call modsub1(dtp1)
  if(.not. allocated(dtp1))             error stop 10_4
  if(dtp1%l1 /= 2)                      error stop 11_4
  if(dtp1%l2 /= 3)                      error stop 12_4

  call modsub1(dtp1)
  if(allocated(dtp1))                   error stop 15_4

  call modsub2(dtp2)
  if(.not. associated(dtp2))            error stop 16_4
  if(dtp2%l1 /= 2)                      error stop 17_4
  if(dtp2%l2 /= 3)                      error stop 18_4
  
  call modsub2(dtp2)
  if(associated(dtp2))                  error stop 21_4

  call extsub1(dtp3)
  if(.not. allocated(dtp3))             error stop 22_4    
  if(dtp3%l1 /= 2)                      error stop 23_4
  if(dtp3%l2 /= 3)                      error stop 24_4

  call extsub1(dtp3)
  if(allocated(dtp3))                   error stop 27_4                 

  call extsub2(dtp4)
  if(.not. associated(dtp4))            error stop 28_4
  if(dtp4%l1 /= 2)                      error stop 29_4
  if(dtp4%l2 /= 3)                      error stop 30_4

  call extsub2(dtp4)  
  if(associated(dtp4))                  error stop 33_4

  call intsub1(dtp3)
  if(.not. allocated(dtp3))             error stop 34_4
  if(dtp3%l1 /= 2)                      error stop 35_4
  if(dtp3%l2 /= 3)                      error stop 36_4
  
  call intsub1(dtp3)
  if(allocated(dtp3))                   error stop 39_4

  call intsub2(dtp4)
  if(.not. associated(dtp4))            error stop 40_4
  if(dtp4%l1 /= 2)                      error stop 41_4
  if(dtp4%l2 /= 3)                      error stop 42_4

  call intsub2(dtp4)
  if(associated(dtp4))                  error stop 45_4

  contains

  subroutine intsub1(arg)
    type(dtp(k1=2,k2=4,l1=:,l2=:)),allocatable :: arg(:)

    if(.not. allocated(arg)) then
       allocate(arg(3),source=dtp(2,4,2,3)())
    else
       if(arg%l1 /= 2)                  error stop 37_4
       if(arg%l2 /= 3)                  error stop 38_4
       deallocate(arg)
    end if
  end subroutine

  subroutine intsub2(arg)
    type(dtp(2,4,:,:)),pointer :: arg
    
    if(.not. associated(arg) ) then
       allocate(arg,source=dtp(2,4,2,3)())
    else
       if(arg%l1 /= 2)                  error stop 43_4
       if(arg%l2 /= 3)                  error stop 44_4
       deallocate(arg)
    end if
  end subroutine   
end program

  subroutine extsub1(arg)
     use m

     type(dtp(2,4,l1=:,l2=:)),allocatable :: arg(:)

     if(.not. allocated(arg)) then
        arg=[dtp(2,4,2,3)(),dtp(2,4,2,3)(),dtp(2,4,2,3)()]
     else
        if(arg%l1 /= 2)                error stop 25_4
        if(arg%l2 /= 3)                error stop 26_4
        deallocate(arg) 
     end if
  end subroutine

  subroutine extsub2(arg)
     use m

     type(dtp(2,4,:,:)),pointer       :: arg
     
     if(.not. associated(arg)) then
        arg=>tar1
     else
        if(arg%l1 /= 2)                error stop 31_4
        if(arg%l2 /= 3)                error stop 32_4

        arg=>null() 
     end if 
  end subroutine
