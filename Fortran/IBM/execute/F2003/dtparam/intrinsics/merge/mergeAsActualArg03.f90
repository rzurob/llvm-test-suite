!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : mergeAsActualArg03.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Sept. 18 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : INTRINSICS(MERGE)
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
!* 1. TEST SECTION 13.7.75 
!* 2. INTRINSICS:MERGE(TSOURCE,FSOURCE,MASK) 
!* 3. TSOURCE AND FSOURCE ARE CHARACTER OR INTEGER COMPONENT
!* 4. LOGICAL COMPONENT AS MASK
!* 5. MERGE AS ARGUMENT OF ELMENTAL FUNCTION
!* 6. DEFECT 356173 356435 356423
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type A(k,l)
     integer,kind :: k
     integer,len  :: l
     
     integer(k)   :: i1(2)
     character(l) :: c1(2)
     logical      :: l1(3)
   end type

   contains

elemental function getChar(ch)
    character(*),intent(in) :: ch
    character(4) :: getChar

    getChar=ch
end function

elemental function getInt(int)
    integer,intent(in) :: int 
    integer :: getInt

    getInt=int  
end function
end module

program mergeAsActualArg03
   use m
   implicit none

   type(A(4,:)),allocatable :: a1
 
   integer,allocatable :: int(:)

   character(:),allocatable :: ch(:)
 
   allocate(a1,source=A(4,4)(i1=[1,2],c1=["abc","def"],& 
                               l1=[.false.,.true.,.false.])) 

   ch=getChar(a1%c1)
   ch=getchar(merge(getchar(ch),ch,.true.))

   if(ch%len /= 4)                                         error stop 10_4
   if(size(ch,1) /= 2)                                     error stop 11_4
   if(any(ch /= ["abc","def"]))                            error stop 12_4

   ch=getchar(merge([a1%c1,"ghi "],"123",a1%l1))
   
   if(ch%len /= 4)                                         error stop 13_4
   if(size(ch,1) /= 3)                                     error stop 14_4
   if(any(ch /= ["123","def","123"]))                      error stop 15_4


   ch=getchar(merge([a1%c1,"ghi "],"123",.not. a1%l1))

   if(ch%len /= 4)                                         error stop 16_4
   if(size(ch,1) /= 3)                                     error stop 17_4
   if(any(ch /= ["abc","123","ghi"]))                      error stop 18_4 

   ch=getchar(merge([a1%c1(:)(1:2),"6"//"7"], &
                      "123567"(1:2),.not. a1%l1))

   if(ch%len /= 4)                                         error stop 19_4
   if(size(ch,1) /= 3)                                     error stop 20_4
   if(any(ch /= ["ab","12","67"]))                         error stop 21_4

   int=getInt(merge(a1%i1,[3,4],[.true.,.false.])) 

   if(size(int,1) /= 2)                                     error stop 22_4
   if(any(int /= [1,4]))                                    error stop 23_4

   int=getInt(merge([a1%i1,3]+1,[4,5,6]+3,.not. a1%l1))

   if(size(int,1) /= 3)                                     error stop 24_4
   if(any(int /= [2,8,4]))                                  error stop 25_4
  
end program

