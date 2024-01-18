!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept. 19 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : INTRINSICS(MERGE)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 13.7.75
!* 2. INTRINSICS:MERGE(TSOURCE,FSOURCE,MASK)
!* 3. ELEMENTAL FUNCTION RESULT IS MERGE
!* 4. TSOURCE AND FSOURCE ARE DERIVED TYPE ARRAY
!* 5. COMPONENTS ARE CHARACTER,INTEGER,LOGICAL
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type dtp(k,l)
     integer,kind :: k
     integer,len  :: l
     integer(k)   :: i1
     character(l) :: c1
     logical      :: l1
  end type

  contains
elemental function getMergeResult1(Ts,Fs,Mask)
        type(dtp(4,*)),intent(in) :: Ts,Fs
        logical,intent(in)        :: Mask
        type(dtp(4,3)) :: getMergeResult1

            getMergeResult1=merge(Ts,Fs,Mask)
     end function
end module

program mergeAsFunRes02
   use m
   implicit none

   integer :: i
   type(dtp(4,3)) :: dtp1(6),dtp2(6)
   type(dtp(4,3)) :: dtp3(2,3),dtp4(2,3)

   character(3)   :: ch1(6)=["a1","b2","c3","d4","e5","f6"]
   integer        :: i1(6)=(/(i,i=1,6)/)

   character(3)   :: ch2(6)=["xx","ll","ff","tt","ee","ss"]
   integer        :: i2(6)=(/(i+10,i=1,6)/)

   logical        :: l1(6)=[.true.,.false.,.true.,.false.,.true.,.false.]
   logical        :: l2(6)

   l2=.not. l1

   dtp1=(/( dtp(4,3)(i1(i),ch1(i),l1(i)) ,i=1,6) /)

   dtp2=(/( dtp(4,3)(i2(i),ch2(i),l2(i)) ,i=1,6) /)

   dtp3=reshape(source=dtp1,shape=(/2,3/))

   dtp4=reshape(source=dtp2,shape=(/2,3/))

   call check1(getMergeResult1(dtp1,dtp2, dtp1%l1))
   call check2(getMergeResult1(dtp3,dtp4, dtp3%l1))

   contains
      subroutine check1(dtp)
         type(dtp(4,*)),intent(in) :: dtp(:)

         if(dtp%k /= 4)                              error stop 10_4
         if(dtp%l /= 3)                              error stop 11_4
         if(any(dtp%i1 /= [1,12,3,14,5,16]))         error stop 12_4
         if(any(dtp%c1 /= ["a1","ll","c3","tt","e5","ss"]))   error stop 13_4
         print *,dtp%l1
         if(any(dtp%l1 .neqv. .true.))               error stop 14_4

      end subroutine

      subroutine check2(dtp)
         type(dtp(4,*)),intent(in) :: dtp(:,:)
         if(dtp%k /= 4)                              error stop 15_4
         if(dtp%l /= 3)                              error stop 16_4
         if(any(dtp(1,:)%i1 /= [1,3,5]))              error stop 17_4
         if(any(dtp(2,:)%i1 /= [12,14,16]))           error stop 18_4
         if(any(dtp(1,:)%c1 /= ["a1","c3","e5"]))     error stop 19_4
         if(any(dtp(2,:)%c1 /= ["ll","tt","ss"]))     error stop 20_4
         if(any(dtp%l1 .neqv. .true.))               error stop 21_4


      end subroutine
end program

