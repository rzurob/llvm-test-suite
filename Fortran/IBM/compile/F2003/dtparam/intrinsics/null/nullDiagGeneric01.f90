!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : nullDiagGeneric01.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Sept. 27 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : NULL([MOLD]) 
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
!* 1. TEST SECTION 13.7.88 
!* 2. ACTUAL ARGUMENTS TO GENERIC PROCEDURE ARE DIFFERENT TYPE , TYPE PARAMETER AND RANK
!* 3. ACTUAL ARGUMENT IS NULL(),MOLD IS ABSENT,SHOULD HAVE DIAGNOSTIC MESSAGE
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type typeA(k)
      integer,kind :: k=4
   end type
   type typeB(k)
      integer,kind :: k=4
   end type

   interface sub
     subroutine sub1(dt)
       import
       type(typeA(4)),pointer :: dt
     end subroutine
     subroutine sub2(dt)
       import
       type(typeA(2)),allocatable :: dt
     end subroutine
     subroutine sub3(dt)
       import
       type(typeB(4)),pointer  :: dt
     end subroutine
     subroutine sub4(dt)
       import
       type(typeB(2)),allocatable :: dt
     end subroutine
     subroutine sub5(dt)
       import
       type(typeB(4)),pointer  :: dt(:)
     end subroutine
     subroutine sub6(dt)
       import
       type(typeB(4)),pointer  :: dt(:,:)
     end subroutine
   end interface

end module

program nullDiagGeneric01
   use m
   implicit none

   type(typeA(4)),pointer     :: a1
   type(typeA(2)),allocatable :: a2
   type(typeB(4)),pointer     :: b1
   type(typeB(2)),allocatable :: b2
   type(typeB(4)),pointer     :: b3(:)
   type(typeB(4)),pointer     :: b4(:,:)

   call sub(null())
   call sub(null())
   call sub(null())
   call sub(null())   
   call sub(null())
   call sub(null())
   
end program

      subroutine sub1(dt)
         use m,only : typeA
         type(typeA(4)),pointer :: dt
      end subroutine

      subroutine sub2(dt)
         use m,only : typeA
         type(typeA(2)),allocatable :: dt
      end subroutine 

      subroutine sub3(dt)
         use m,only : typeB
         type(typeB(4)),pointer :: dt
      end subroutine

      subroutine sub4(dt)
         use m,only : typeB
         type(typeB(2)),allocatable :: dt
      end subroutine

      subroutine sub5(dt)
         use m,only : typeB
         type(typeB(4)),pointer :: dt(:)
      end subroutine

      subroutine sub6(dt)
         use m,only : typeB
         type(typeB(4)),pointer :: dt(:,:)
      end subroutine
