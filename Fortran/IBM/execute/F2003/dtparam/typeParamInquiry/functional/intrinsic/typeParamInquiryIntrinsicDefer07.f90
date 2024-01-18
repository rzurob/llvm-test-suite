!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : typeParamInquiryIntrinsicDefer07.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : July 31 2008  
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
!* 2. TYPE PARAMETER INQUIRY FOR INTRINSIC TYPE 
!* 3. DEFERRED TYPE PARAMETER INQUIRY
!* 4. ALLOCATABLE AND POINTER  
!* 5. DUMMY ARGUMENT IS UNLIMITED POLYMORPHIC 
!* 6. DEFECT 354520
!234567890123456789012345678901234567890123456789012345678901234567890

program typeParamInquiryIntrinsicDefer07

    implicit none

    character(:),allocatable :: a1(:)
    integer(2),allocatable :: b1
    character(:),pointer :: a2(:)
    integer(8),pointer :: b2

    allocate(a1(len("xlf")+1),source=["abc","def","ghi","jkl"])

    call test1(a1) 

    allocate(character(ubound(a1,1)+lbound(a1,1) + &
             a1%len + len(a1) + a1%kind+ kind(a1) ) :: a2(a1%len+len(a1)) )

    call test2(a2)
   
    call test3(b1)

    call test4(b2)
 
    contains
      subroutine test1(x)
        class(*),intent(in)  :: x(:)
          
        select type(x)
          type is (character(*))
            if(x%len /=len(x) .or. x%len /= 3)         error stop 10_4
            if(x%kind /=kind(x) .or. x%kind /=1)       error stop 11_4
            if(ubound(x,1) /= 4)                       error stop 12_4
          class default
            error stop 100_4
        end select  
      end subroutine 

      subroutine test2(x)
        class(*),intent(in)  :: x(:)

        select type(x)
          type is (character(*))
            if(x%len /=len(x) .or. x%len /= 13)         error stop 13_4
            if(x%kind /=kind(x) .or. x%kind /=1)        error stop 14_4
            if(ubound(x,1) /= 6)                        error stop 15_4
          class default
            error stop 101_4
        end select
      end subroutine

      subroutine test3(x)
        class(*),intent(in)  :: x

        select type(x)
          type is (integer(2))
            if(x%kind /=kind(x) .or. x%kind /=2)       error stop 16_4
          class default
            error stop 102_4
        end select
      end subroutine

      subroutine test4(x)
        class(*),intent(in)  :: x

        select type(x)
          type is (integer(8))
            if(x%kind /=kind(x) .or. x%kind /=8)        error stop 17_4
          class default
            error stop 103_4
        end select
      end subroutine

end

